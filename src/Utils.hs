{-# LANGUAGE ScopedTypeVariables #-}

module Utils where

import System.IO
import Control.Concurrent.Extra
import Control.Exception.Extra as E
import GHC.IO.Handle(hDuplicate,hDuplicateTo)
import System.Directory.Extra
import System.IO.Error
import System.FilePath
import Data.Char
import Data.Time.Clock

import Development.Shake.Fancy hiding (withTempDir)


-- This is copied from extra and shake sources, because they do not allow to
-- specify the temp directory
newTempDir :: FilePath -> IO (FilePath, IO ())
newTempDir tmpdir = do
        dir <- create
        del <- once $ ignore $ removeDirectoryRecursive dir
        return (dir, del)
    where
        create = do
            -- get the number of seconds during today (including floating point), and grab some interesting digits
            rand :: Integer <- fmap (read . take 20 . filter isDigit . show . utctDayTime) getCurrentTime
            find tmpdir rand

        find tmpdir x = do
            let dir = tmpdir </> "dht-" ++ show x
            catchBool isAlreadyExistsError
                (createDirectoryPrivate dir >> return dir) $
                \e -> find tmpdir (x+1)

withTempDir :: FilePath -> (FilePath -> Action a) -> Action a
withTempDir tmpdir act = do
    (dir,del) <- liftIO $ newTempDir tmpdir
    act dir `actionFinally` del

withTempDirIO :: FilePath -> (FilePath -> IO a) -> IO a
withTempDirIO tmpdir act = do
    (dir,del) <- newTempDir tmpdir
    act dir `finally` del

