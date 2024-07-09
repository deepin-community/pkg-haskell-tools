{-# LANGUAGE GeneralizedNewtypeDeriving, DeriveDataTypeable, RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

import qualified Data.Map as M
import qualified Data.Set as S
import Control.Applicative hiding (many)
import qualified Data.Text as T
import Data.Char (isSpace)
import Data.List
import Data.List.Split
import Data.Maybe
import Data.Monoid
import Control.Monad
import Text.Read
import System.Directory (createDirectory, copyFile)
import qualified System.Directory
import System.Directory.Extra (listFiles, listContents)
import System.Exit
import System.Environment

import Options.Applicative hiding (many)
import qualified Options.Applicative as O
import Options.Applicative.Types (readerAsk)

import Development.Shake.Fancy hiding (withTempDir)
import qualified Development.Shake.Fancy as S
import Development.Shake.Classes
import Development.Shake.FilePath

import Debian.Relation.Common
import Debian.Control.String
import Debian.Control.Policy

import Text.Parsec hiding (option, oneOf)
import Text.Parsec.String

import Utils
import Link

-- Option parsing
data Conf = Conf
    { excludedPackages :: [String]
    , targetDir :: FilePath
    , bindMountDir :: FilePath
    , jobs :: Int
    , schrootName :: String
    , shakeVerbosity' :: Verbosity
    , keepGoing :: Bool
    , sbuildArgs :: [String]
    , targets :: [String]
    }

confSpec :: O.Parser Conf
confSpec = Conf
 <$> option parseCommaOrSpace (
    long "excluded-packages" <>
    metavar "PKG,PKG,..." <>
    help "comma or space separated list of source package names to ignore" <>
    value defaultExcludedPackages <>
    showDefaultWith (intercalate ", ")
    )
 <*> strOption (
    long "output" <>
    short 'o' <>
    metavar "DIR" <>
    help "output directory" <>
    showDefault <>
    value "lab"
    )
 <*> strOption (
    long "bindmount" <>
    metavar "DIR" <>
    help "directory bind-mounted in the schroot" <>
    showDefault <>
    value "/tmp"
    )
 <*> option parseNat (
    long "jobs" <>
    short 'j' <>
    metavar "INT" <>
    help "number of parallel jobs" <>
    showDefault <>
    value 1
    )
 <*> strOption  (
    long "chroot" <>
    short 'c' <>
    metavar "SCHROOT" <>
    help "name of the schroot to use" <>
    showDefault <>
    value "haskell"
    )
 <*> option readOption  (
    long "shake-verbosity" <>
    metavar "VERBOSITY" <>
    help "verbosity for shake (Silent, Quiet, Normal, Loud, Chatty or Diagnostic)" <>
    showDefault <>
    value Normal
    )
 <*> switch  (
    long "keep-going" <>
    help "keep going even if there are errors" <>
    showDefault
    )
 <*> O.many ( strOption (
    long "sbuild-option" <>
    metavar "OPTION" <>
    help "An option to pass on to sbuilder (can be passed multiple times)"
    ))
 <*> O.many (argument str (metavar "TARGET..."))

parseCommaOrSpace:: ReadM [String]
parseCommaOrSpace = do
    s <- readerAsk
    return $ split (dropBlanks $ dropDelims $ oneOf ";, ") s

readOption :: Read a => ReadM a
readOption  = do
    s <- readerAsk
    case readMaybe s of
        Nothing -> fail $ "Cannot parse " ++ s
        Just n -> return n

parseNat :: ReadM Int
parseNat  = do
    s <- readerAsk
    case readMaybe s of
        Nothing -> fail "Not a number"
        Just n | n < 0 -> fail "I cannot do a negative number of jobs"
               | otherwise -> return n

-- dpkg-parsechangelog is slow, so here is a quick hack
-- TODO: Ensure this is not run unnecessarily often
versionOfSource :: String -> Action String
versionOfSource s = do
    let f = "p" </> s </> "debian" </> "changelog"
    need [f]
    ret <- liftIO $ parseFromFile p f
    case ret of
        Left e -> fail (show e)
        Right s -> return s
  where
    p = do
        many $ noneOf "("
        char '('
        v <- many1 $ noneOf ")"
        char ')'
        return (removeEpoch v)

distributionOfSource :: String -> Action String
distributionOfSource s = do
    let f = "p" </> s </> "debian" </> "changelog"
    need [f]
    ret <- liftIO $ parseFromFile p f
    case ret of
        Left e -> fail (show e)
        Right s -> return s
  where
    p = do
        many $ noneOf "("
        char '('
        many1 $ noneOf ")"
        char ')'
        char ' '
        many1 $ noneOf " ;\n"


-- Splits the filename of a .deb, .changes or .build file into version, source
-- and architecture
splitDebName :: String -> (String, String, String)
splitDebName filename
    | [pkgname, version, arch] <- splitOn "_" (dropExtension filename)
    = (pkgname, version, arch)
    | otherwise
    = error $ "splitDebName: Unexpected filename " ++ show filename

ensureVersion :: String -> String -> Action ()
ensureVersion s v = do
    ex <- doesFileExist $ "p" </> s </> "debian" </> "changelog"
    unless ex $
        fail $ "I do not know about package " ++ s
    v' <- versionOfSource s
    when (v /= v') $
        fail $ "Cannot build " ++ s ++ " version " ++ v ++ ", as we have " ++ v' ++ "."

ensureArch :: String -> Action ()
ensureArch a = do
    a' <- askOracle (GetArch ())
    case () of
      () | a == a'    -> return ()
         | a == "all" -> return ()
         | otherwise  -> fail $ "Demanded architecture " ++ a ++ " does not match schroot architecture " ++ a' ++ "."

removeEpoch :: String -> String
removeEpoch s | ':' `elem` s = tail $ dropWhile (/= ':') s
              | otherwise    = s


changesFileName s v a = s ++ "_" ++ v ++ "_" ++ a ++ ".changes"
logFileName s v a = s ++ "_" ++ v ++ "_" ++ a ++ ".build"
sourceFileName s v = s ++ "_" ++ v ++ ".dsc"

binaryPackagesOfSource :: String -> String -> Action [(String, String)]
binaryPackagesOfSource s arch = do
    let controlFile = "p" </> s </> "debian" </> "control"
    need [controlFile]
    ret <- liftIO $ parseDebianControlFromFile controlFile
    case ret of
        Left e -> fail (show e)
        Right dc -> forM (debianBinaryParagraphs dc) $  \bp -> do
            p <- maybe (fail "No Package field") (return . T.unpack) $ fieldValue "Package" bp
            a <- maybe (fail "No Arch field") (return . T.unpack) $ fieldValue "Architecture" bp
            return (p, if a == "all" then a else arch)

dependsOfDsc :: FilePath -> IO [String]
dependsOfDsc f = do
    ret <- parseControlFromFile f
    case ret of
        Left e -> fail (show e)
        Right (Control (p:_)) -> do
            deps <- case fieldValue "Build-Depends" (p:: Paragraph) of
                Nothing -> fail "no Build-Depends"
                Just depV -> return $ nub $ parseFlatRel depV
            ideps <- case fieldValue "Build-Depends-Indep" (p:: Paragraph) of
                Nothing -> return []
                Just depV -> return $ nub $ parseFlatRel depV
            return $ deps ++ ideps


-- Parsing package relations with flattening
-- (this could be faster than flatRels . parseRels)
parseFlatRel :: String -> [String]
parseFlatRel = flatRels . parseRels
  where
    flatRels :: Relations -> [String]
    flatRels = map (\(Rel (BinPkgName n) _ _) -> n) . join

    parseRels :: String -> Relations
    parseRels s = case parseRelations s of
      Left pe ->  error $ "Failed to parse relations " ++ show pe
      Right rel -> rel

fixupScript :: [String] -> String
fixupScript pkgs = unlines $
    [ "#!/bin/bash"
    -- I disable locking in the schroot. I have /var/cache/apt/archives bind-mounted
    -- via /etc/schroot/default/fstab, so with locking, I could not run
    -- multiple sbuilds at the same time
    , "echo 'Debug::NoLocking \"true\";' > /etc/apt/apt.conf.d/no-locking"
    ] ++ ignoreArchiveDepends pkgs

ignoreArchiveDepends :: [String] -> [String]
ignoreArchiveDepends [] = []
ignoreArchiveDepends pkgs =
    [ "#!/bin/bash"
    , "apt-get install dctrl-tools" -- Just in case it is not installed in the base schroot
    , "for f in /var/lib/apt/lists/*_Packages"
    , "do"
    , "grep-dctrl -v -F Package -X \\( " ++ disj ++ " \\) < \"$f\" > \"$f\".tmp"
    , "mv \"$f\".tmp \"$f\""
    , "done"
    ]
  where disj = intercalate " -o " pkgs


debFileNameToPackage filename =
    let [pkgname,_version,_] = splitOn "_" filename
    in pkgname

defaultExcludedPackages = words "ghc ghc-testsuite haskell-devscripts haskell98-report haskell-platform"
newtype GetExcludedSources = GetExcludedSources () deriving (Show,Typeable,Eq,Hashable,Binary,NFData)

newtype GetDebBuiltBy = GetDebBuiltBy String  deriving (Show,Typeable,Eq,Hashable,Binary,NFData)
type instance RuleResult GetDebBuiltBy = Maybe String
newtype GetBinToDeb = GetBinToDeb String  deriving (Show,Typeable,Eq,Hashable,Binary,NFData)
type instance RuleResult GetBinToDeb = Maybe String

newtype GetArch = GetArch () deriving (Typeable,Eq,Hashable,Binary,NFData)
type instance RuleResult GetArch = String
instance Show GetArch where show (GetArch ()) = "querying architecture"

-- Find dependencies on binary packages we build ourselves.
-- This is needed, because those binary packages don't need to be
-- build-dependencies of the package that needs them at runtime
-- and might not be built yet.
-- Finding library packages isn't necessary, because they were already
-- built as a build-dependency.
extendBuildDepends :: FilePath -> (String -> Action (Maybe String)) -> [String] -> [String] -> Action [String]
extendBuildDepends targetDir binToDeb ourBins buildDeps = do
    let notOurBin = map (targetDir </>) $ filter (`notElem` ourBins) buildDeps
    -- First need all
    need notOurBin
    -- The look for additional dependencies
    (buildDeps ++) <$> concat <$> mapM ourDepends notOurBin
  where
    ourDepends debFile = do
        Stdout depList <- cmdWrap "dpkg-deb --field Depends" $ cmd
                              [ "dpkg-deb"
                              , "--field"
                              , debFile
                              , "Depends"
                              ]
        let depPkgs = map cleanDep $ splitOn "," depList
        depends <- catMaybes <$> mapM binToDeb depPkgs
        recursive <- extendBuildDepends targetDir binToDeb ourBins depends
        return $ depends ++ recursive

    cleanDep = takeWhile (not . isSpace) . dropWhile isSpace

manpage :: String
manpage = unlines [ "TODO" ]

main = do
    args <- getArgs
    case args of
        ["--manpage"] -> do
            let failure = parserFailure (prefs idm) opts (ShowHelpText Nothing) mempty
            let message = renderFailure failure "dht make-all"
            putStrLn . unlines . drop 2 . lines . fst $ message
        _ -> execParser opts >>= run
  where
    opts = info (helper <*> confSpec)
        ( fullDesc
       <> progDesc "Rebuilds a set of packages"
       <> header "make-all - Rebuilds a set of packages" )

    run conf = do
         packages <- getSourcePackages $ excludedPackages conf
         testBindMount conf $ shake (makeShakeOptions conf) (shakeMain conf packages)

testBindMount :: Conf -> IO () -> IO ()
testBindMount (Conf {..}) act = do
    -- Test if the bindmount directory works
    withTempDirIO bindMountDir $ \tmpdir -> do
        Exit c <- cmd ["schroot", "-d", "/", "-c", schrootName, "--", "test", "-d", tmpdir]
        if (c == ExitSuccess)
        then act
        else putStrLn $ unlines
            [ "Inside the schroot " ++ schrootName ++", the directory " ++ bindMountDir
            , "is not available. Please configure the schroot to bindmount this directory"
            , "(e.g. in /etc/schroot/default/fstab), or choose a different directory using"
            , "the --bindmount parameter."
            , ""
            , "For best performance, choose a directory on a tmpfs file system or"
            , "the same partition as --output (currently " ++ targetDir ++ ")."
            , "Actually, the latter optimization is not yet implemented...."
            ]

getSourcePackages :: [FilePath] -> IO [FilePath]
getSourcePackages excluded = do
  sources <- map takeFileName <$> listContents "p" -- Filtering out files can be avoided, because a file can't contain a debian directory and will be filtered out in the next step.
  let sources' = filter (`notElem` excluded) sources
  flip filterM sources' $ \s ->
      (&&) <$> System.Directory.doesFileExist ("p" </> s </> "debian/control")
           <*> System.Directory.doesFileExist ("p" </> s </> "debian/changelog")

makeShakeOptions :: Conf -> ShakeOptions
makeShakeOptions Conf{..} = shakeOptions
    { shakeFiles = targetDir </> ".shake"
    , shakeThreads = jobs
    , shakeVerbosity = shakeVerbosity'
    , shakeChange = ChangeModtimeAndDigestInput
    , shakeStaunch = keepGoing
    , shakeProgress = progressSimple
    }

shakeMain conf@(Conf {..}) sources = do
    if null targets then want ["all"] else want targets

    getArch' <- addOracle $ \GetArch{} -> do
        Stdout archString <- cmdWrap "schroot" $ cmd
            [ "schroot"
            , "-d", "/"
            , "-c", schrootName
            , "--"
            , "dpkg", "--print-architecture"
            ]
        case lines archString of
            [a] -> return a
            _ -> fail $ "Unexpected output from dpkg --print-architecture: \""++ archString ++"\""
    let getArch = getArch' (GetArch ())

    targetDir </> "cache/all-binaries.txt" %> \out -> do
        binaries <- concat <$> mapM readFileLines
            [ targetDir </> "cache" </> "binaries" </> s <.> "txt" | s <- sources ]
        putNormal "# enumerating all binaries..."
        writeFileChanged out $ unlines binaries

    targetDir </> "cache/built-by.txt" %> \out -> do
        builtBy <- liftM (sort . concat) $ forM sources $ \s -> do
            pkgs <- readFileLines $ targetDir </> "cache/binaries/" ++ s ++ ".txt"
            return [(pkg,s) | pkg <- pkgs]
        putNormal "# building built-by cache..."
        writeFileChanged out $ unlines [ unwords [pkg,s] | (pkg,s) <- builtBy ]

    -- This maps binary package _file_ names built by us to source package names
    debBuiltByMap <- newCache $ \() -> do
        builtBy <- readFileLines $ targetDir </> "cache/built-by.txt"
        return $ M.fromList [ (deb, source)
            | [deb,source] <- words <$> builtBy
            ]

    getDebBuiltBy <- addQuietOracle $ \(GetDebBuiltBy bin) -> do
        map <- debBuiltByMap ()
        return $ M.lookup bin map

    let debBuiltBy :: String -> Action (Maybe String)
        debBuiltBy = getDebBuiltBy . GetDebBuiltBy

    -- This maps binary _package_ names built by us to binary packages _filenames_
    binToDebMap <- newCache $ \() -> do
        builtBy <- readFileLines $ targetDir </> "cache/built-by.txt"
        return $ M.fromList [ (pkgname, deb)
            | [deb,_source] <- words <$> builtBy
            , let (pkgname,_,_) = splitDebName deb
            ]

    getBinToDeb <- addQuietOracle $ \(GetBinToDeb bin) -> M.lookup bin <$> binToDebMap ()

    let binToDeb :: String -> Action (Maybe String)
        binToDeb = getBinToDeb . GetBinToDeb

    targetDir </> "cache/all-changes-files.txt" %> \out -> do
        arch <- getArch
        putNormal "# enumerating all changes files..."
        versioned <- forM sources $ \s -> do
            v <- versionOfSource s
            return (s,v)
        writeFileChanged out $ unlines $ map (flip (uncurry changesFileName) arch) versioned

    targetDir </> "cache/binaries/*.txt" %> \out -> do
        let s = dropExtension $ takeFileName out
        putNormal $ "# enumerating binaries of " ++ show s
        arch <- getArch
        pkgs <- binaryPackagesOfSource s arch
        v <- versionOfSource s

        writeFileChanged out $ unlines [ p ++ "_" ++ v ++ "_" ++ a ++ ".deb" | (p,a) <- pkgs]

    "all" ~> do
        changesFiles <- readFileLines $ targetDir </> "cache/all-changes-files.txt"
        need [ targetDir </>  l | l <- changesFiles]

    "all-sources" ~> do
        sources <- readFileLines $ targetDir </> "cache/sources.txt"
        versioned <- forM sources $ \s -> do
            v <- versionOfSource s
            return (s,v)
        need [ targetDir </> sourceFileName s v | (s,v) <- versioned ]

    -- Binary packages depend on the corresponding changes file log
    targetDir </> "*.deb" %> \out -> do
        let filename = takeFileName out
        let (_pkgname,version,binArch) = splitDebName filename
        ensureArch binArch -- binArch could be all, so do not use it later
        arch <- askOracle (GetArch ())
        sourceMB <- debBuiltBy filename
        case sourceMB of
            Nothing -> fail $ "File " ++ filename ++ " not built by us."
            Just source -> need [targetDir </> changesFileName source version arch]

    -- Changes files depend on the corresponding log file
    targetDir </> "*.changes" %> \out -> do
        let filename = takeFileName out
        let (source,version,arch) = splitDebName filename
        ensureArch arch
        let logfile = targetDir </> logFileName source version arch
        need [logfile]
        ok <- doesFileExist out
        unless ok $ do
            putNormal $ "Building " ++ source ++ " failed, remove " ++ logfile ++ " to retry."
            fail ""

    -- Build log depends on the corresponding source, and the dependencies
    targetDir </> "*.build" %> \out -> do
        let filename = takeFileName out
        let (source,version,arch) = splitDebName filename
        ensureArch arch
        let changes = changesFileName source version arch

        ensureVersion source version

        -- To build something, we need the source
        let dsc = sourceFileName source version
        need [targetDir </> dsc]

        -- This ensures all dependencies are up-to-date
        deps <- liftIO $ dependsOfDsc $ targetDir </> dsc
        putLoud $ "# " ++ source ++ " depends on:"
        putLoud $ "# packages: " ++ intercalate "," deps
        depFileNames <- catMaybes <$> mapM binToDeb deps
        putLoud $ "# dependencies: " ++ intercalate "," depFileNames

        ourBins <- readFileLines $ targetDir </> "cache" </> "binaries" </> source <.> "txt"

        depFileNames' <- extendBuildDepends targetDir binToDeb ourBins depFileNames
        putLoud $ "# recursive dependencies: " ++ intercalate "," depFileNames'

        -- For the sake of packages like alex, uuagc etc, we exclude ourselves
        -- from this, thus allowing the use of the binary from the archive to
        -- bootstrap.
        need $ map (targetDir </>) $ filter (`notElem` ourBins) depFileNames'

        -- What files do we have built locally?
        -- Make sure the build uses only them
        -- Do not use readFileLines or readFile', lest we rebuild everything
        -- if the set of packages changes.
        orderOnly [targetDir </> "cache" </> "all-binaries.txt"]
        expectedDeps <- S.fromList . lines <$> liftIO (readFile (targetDir </> "cache" </> "all-binaries.txt"))
        localDebs <-
            filter (`S.member` expectedDeps) .
            map (makeRelative targetDir) <$>
            liftIO (listFiles targetDir)
        let localDepPkgs = map debFileNameToPackage localDebs
        withTempDir bindMountDir $ \tmpdir -> do
            -- Now monkey-patch dependencies out of the package lists
            let fixup = tmpdir </> "fixup.sh"
            liftIO $ writeFile fixup  $ fixupScript localDepPkgs

            -- Create a dummy repository
            let repoDir = tmpdir </> "repo"
            liftIO $ createDirectory repoDir
            forM_ localDebs $ \p -> liftIO $ linkOrCopyFile ("lab" </> p) (repoDir </> p)
            unit $ cmdWrap "dpkg-scanpackages" $ cmd
                (Cwd repoDir)
                (EchoStderr False)
                (FileStdout (repoDir </> "Packages"))
                ["dpkg-scanpackages", "."]

            -- Always pass the distribution in the changelog to sbuild, otherwise
            -- sbuild will put "unstable" into the changes file, even if we do
            -- not build for unstable.
            -- See https://bugs.debian.org/529281 for why sbuild doesn't do it
            -- for us.
            dist <- distributionOfSource source

            -- Run sbuild
            Exit c <- cmdWrap "sbuild" $ cmd
                (Cwd targetDir)
                (EchoStdout False) $
                ["sbuild"
                , "-c", schrootName
                , "-A"
                , "--no-apt-update"
                , "--dist", dist
                , "--chroot-setup-commands=bash " ++ fixup
                , "--extra-repository=deb [trusted=yes] file://" ++ repoDir ++ " ./"
                , dsc
                ] ++ sbuildArgs
            if c == ExitSuccess
            then
                -- Add the sources to the changes file. We do not simply pass
                -- "-s" to sbuild, as it would make sbuild re-create and override the .dsc file
                -- which confuses the build system.
                unit $ cmdWrap "changestool" $
                    cmd ["changestool", targetDir </> changes, "adddsc", targetDir </> dsc]
            else do
                putNormal $ "Failed to build " ++ source ++ "_" ++ version
                ex <- liftIO $ System.Directory.doesFileExist out
                putNormal $ "See " ++ out ++ " for details."


    -- Build log depends on the corresponding source, and the dependencies
    targetDir </> "*.dsc" %> \out -> do
        let filename = dropExtension $ takeFileName out
        let [source,version] = splitOn "_" filename
        ensureVersion source version
        sourceFiles <- getDirectoryFiles ("p" </> source) ["debian//*"]
        need [ "p" </> source </> f | f <- sourceFiles]
        unit $ cmdWrap "debian2dsc" $ cmd (EchoStdout False)
                "dht" "debian2dsc" "-o" targetDir ("p" </> source </> "debian")

    phonys $ \source -> if source `elem` sources
                        then Just $ do
                          version <- versionOfSource source
                          arch <- getArch
                          need [targetDir </> changesFileName source version arch]
                        else Nothing
