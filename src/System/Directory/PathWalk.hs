module System.Directory.PathWalk
    ( pathWalk
    ) where

import Control.Monad (forM_, filterM)
import Data.IORef (newIORef, readIORef, writeIORef)
import System.Directory (doesDirectoryExist, doesFileExist, getDirectoryContents)
import System.FilePath ((</>))

pathWalk :: FilePath -> (FilePath -> [FilePath] -> [FilePath] -> IO ()) -> IO ()
pathWalk root callback = do
    names <- getDirectoryContents root
    let properNames = filter (`notElem` [".", ".."]) names

    dirs <- filterM (\n -> doesDirectoryExist $ root </> n) names
    files <- filterM (\n -> doesFileExist $ root </> n) names

    callback root dirs files

    forM_ dirs $ \dir -> do
        pathWalk (root </> dir) callback
