{-# LANGUAGE CPP #-}
-- | Provides path traversal functions much like Python's os.walk.

module System.Directory.PathWalk
    ( Callback
    , pathWalk
    , WalkStatus(..)
    , pathWalkInterruptible
    , pathWalkAccumulate
    , pathWalkLazy
    ) where

#if __GLASGOW_HASKELL__ < 710
import Data.Monoid (Monoid)
#endif
import Control.Monad (forM, forM_, filterM)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Maybe (MaybeT(..), runMaybeT)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Writer.Lazy (runWriterT, tell)
import System.Directory (doesDirectoryExist, doesFileExist, getDirectoryContents)
import System.FilePath ((</>))
import System.IO.Unsafe (unsafeInterleaveIO)

-- | Called with a directory, list of relative subdirectories, and a
-- list of file names.  If using 'pathWalk', the callback always
-- returns '()'.  If using 'pathWalkInterruptible', it returns whether
-- to continue, prevent recursing further, or stop traversal entirely.
type Callback m a = FilePath -> [FilePath] -> [FilePath] -> m a

-- | 'pathWalk' recursively enumerates the given root directory,
-- calling callback once per directory with the traversed directory
-- name, a list of subdirectories, and a list of files.
--
-- The subdirectories and file names are always relative to the root
-- given.
--
-- @
-- pathWalk "src" $ \\dir subdirs files -> do
--   forM_ files $ \\file -> do
--     when ("Test.hs" \`isSuffixOf\` file) $ do
--       registerTestFile $ dir \</\> file
-- @
pathWalk :: MonadIO m => FilePath -> Callback m () -> m ()
pathWalk root callback = do
  pathWalkInterruptible root $ \dir dirs files -> do
    callback dir dirs files
    return Continue

-- | The callback given to 'pathWalkInterruptible' returns a WalkStatus
-- which determines which subsequent directories are traversed.
data WalkStatus
  = Continue -- ^ Continue recursing all subdirectories.
  | StopRecursing -- ^ Do not traverse deeper.
  | Stop -- ^ Stop recursing entirely.
  deriving (Show, Eq)

readDirsAndFiles :: FilePath -> IO ([FilePath], [FilePath])
readDirsAndFiles root = do
  names <- getDirectoryContents root
  let properNames = filter (`notElem` [".", ".."]) names

  dirs <- filterM (\n -> doesDirectoryExist $ root </> n) properNames
  files <- filterM (\n -> doesFileExist $ root </> n) properNames

  return (dirs, files)

pathWalkInternal :: MonadIO m => FilePath -> Callback m WalkStatus -> m (Maybe ())
pathWalkInternal root callback = do
  (dirs, files) <- liftIO $ readDirsAndFiles root

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

-- | Traverses a directory tree, just like 'pathWalk', except that
-- the callback can determine whether to continue traversal.  See
-- 'WalkStatus'.
pathWalkInterruptible :: MonadIO m => FilePath -> Callback m WalkStatus -> m ()
pathWalkInterruptible root callback = do
  _ <- pathWalkInternal root callback
  return ()


-- | Traverses a directory tree, just like 'pathWalk'.  The difference
-- is that each callback returns a 'Monoid' value, all of which are
-- accumulated into the result.  Note that this uses 'WriterT' and
-- thus frequently appends to the right of the monoid.  Be careful to
-- avoid accidental quadratic behavior by using a data structure that
-- supports fast appends.  For example, use Data.Sequence instead of a
-- list.
pathWalkAccumulate :: (MonadIO m, Monoid o) => FilePath -> Callback m o -> m o
pathWalkAccumulate root callback = do
  ((), result) <- runWriterT $ do
    pathWalk root $ \dir dirs files -> do
      r <- lift $ callback dir dirs files
      tell r
  return result

-- | The lazy version of 'pathWalk'.  Instead of running a callback
-- per directory, it returns a lazy list that reads from the
-- filesystem as the list is evaluated.
--
-- 'pathWalkLazy' does not allow selective recursion.  For richer
-- functionality, see the directory-tree package at
-- https://hackage.haskell.org/package/directory-tree
pathWalkLazy :: MonadIO m => FilePath -> m [(FilePath, [FilePath], [FilePath])]
pathWalkLazy root = liftIO $ unsafeInterleaveIO $ do
  (dirs, files) <- readDirsAndFiles root

  next <- unsafeInterleaveIO $ do
    allsubs <- forM dirs $ \dir -> do
      pathWalkLazy $ root </> dir
    return $ concat allsubs

  return $ (root, dirs, files) : next
