module System.Directory.PathWalk
    ( pathWalk
    ) where

import Control.Monad (forM_, filterM)
import Data.IORef (newIORef, readIORef, writeIORef)
import System.Directory (doesDirectoryExist, doesFileExist, getDirectoryContents)
import System.FilePath ((</>))

-- | 'pathWalk' recursively enumerates the given root directory,
-- calling callback once per directory with the traversed directory name, a list of subdirectories, and a list of files.
--
-- The subdirectories and file names are always relative to the root given.
--
-- @
-- pathWalk "src" $ \\dir subdirs files -> do
--   forM_ files $ \\file -> do
--     when ("Test.hs" \`isSuffixOf\` file) $ do
--       registerTestFile $ dir \</\> file
-- @
pathWalk :: FilePath -> (FilePath -> [FilePath] -> [FilePath] -> IO ()) -> IO ()
pathWalk root callback = do
    names <- getDirectoryContents root
    let properNames = filter (`notElem` [".", ".."]) names

    dirs <- filterM (\n -> doesDirectoryExist $ root </> n) names
    files <- filterM (\n -> doesFileExist $ root </> n) names

    callback root dirs files

    forM_ dirs $ \dir -> do
        pathWalk (root </> dir) callback
