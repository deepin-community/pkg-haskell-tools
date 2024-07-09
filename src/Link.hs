module Link (linkOrCopyFile) where

import Control.Exception
import System.IO.Error
import GHC.IO.Exception

import System.Posix.Files
import System.Directory

linkOrCopyFile :: FilePath -> FilePath -> IO ()
linkOrCopyFile old new = do
    catchJust
        (\e -> if ioeGetErrorType e == UnsupportedOperation then Just () else Nothing)
        (createLink old new)
        (\() -> copyFile old new)
