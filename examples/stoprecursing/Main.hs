module Main (main) where

import Control.Monad (forM_)
import Data.List (isSuffixOf)
import System.Directory.PathWalk (pathWalkInterruptible, WalkStatus(..))
import System.Environment (getArgs)

main :: IO ()
main = do
  rawArgs <- getArgs
  let args = if rawArgs == [] then ["."] else rawArgs
  forM_ args $ \arg -> do
    pathWalkInterruptible arg $ \root dirs files -> do
      if ("/.git" `isSuffixOf` root) then do
        return StopRecursing
      else do
        putStrLn root
        putStrLn $ "  dirs: " ++ show dirs
        putStrLn $ "  files: " ++ show files
        return Continue
