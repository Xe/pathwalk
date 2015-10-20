module System.Directory.PathWalk
    ( Callback
    , pathWalk
    , WalkStatus(..)
    , pathWalkInterruptible
    ) where

import Control.Monad (forM_, filterM)
import System.Directory (doesDirectoryExist, doesFileExist, getDirectoryContents)
import System.FilePath ((</>))
import Control.Monad.Trans.Maybe (MaybeT(..), runMaybeT)

type Callback a = FilePath -> [FilePath] -> [FilePath] -> IO a

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
pathWalk :: FilePath -> Callback () -> IO ()
pathWalk root callback = do
  pathWalkInterruptible root $ \dir dirs files -> do
    callback dir dirs files
    return Continue

data WalkStatus = Continue | StopRecursing | Stop

-- this will never return StopRecursing but I didn't want to create a new data
-- type for that
pathWalkInternal :: FilePath -> Callback WalkStatus -> IO (Maybe ())
pathWalkInternal root callback = do
  names <- getDirectoryContents root
  let properNames = filter (`notElem` [".", ".."]) names

  dirs <- filterM (\n -> doesDirectoryExist $ root </> n) properNames
  files <- filterM (\n -> doesFileExist $ root </> n) properNames

  result <- callback root dirs files
  case result of
    Continue -> do
      runMaybeT $ do
        forM_ dirs $ \dir -> do
          MaybeT $ pathWalkInternal (root </> dir) callback
    StopRecursing -> do
      return $ Just ()
    Stop -> do
      return Nothing

pathWalkInterruptible :: FilePath -> Callback WalkStatus -> IO ()
pathWalkInterruptible root callback = do
  _ <- pathWalkInternal root callback
  return ()
