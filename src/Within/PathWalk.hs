module PathWalk where

import Control.Monad (forM_)
import Data.IORef (newIORef, readIORef, writeIORef)
import System.Directory (doesDirectoryExist, getDirectoryContents)
import System.FilePath ((</>))

pathWalk :: FilePath -> (FilePath -> [FilePath] -> [FilePath] -> IO ()) -> IO ()
pathWalk root callback = do
    dirs  <- newIORef ([] :: [FilePath])
    files <- newIORef ([] :: [FilePath])
    names <- getDirectoryContents root
    let properNames = filter (`notElem` [".", ".."]) names

    forM_ properNames $ \name -> do
        isDir <- doesDirectoryExist $ root </> name

        case isDir of
            True -> do
                val <- readIORef dirs
                writeIORef dirs $ val ++ [name]
            False -> do
                val <- readIORef files
                writeIORef files $ val ++ [name]

    cbDirs  <- readIORef dirs
    cbFiles <- readIORef files

    callback root cbDirs cbFiles

    forM_ cbDirs $ \dir -> do
        let newPath = root </> dir
        pathWalk newPath callback
