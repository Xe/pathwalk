module Main (main) where

import Control.Monad (forM_)
import System.Directory.PathWalk (pathWalkLazy)
import System.Environment (getArgs)

main :: IO ()
main = do
  rawArgs <- getArgs
  let args = if rawArgs == [] then ["."] else rawArgs
  forM_ args $ \arg -> do
    results <- pathWalkLazy arg
    forM_ results $ \(root, dirs, files) -> do
      putStrLn root
      putStrLn $ "  dirs: " ++ show dirs
      putStrLn $ "  files: " ++ show files
