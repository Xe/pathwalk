module Main (main) where

import Control.Monad (forM_)
import System.Directory.PathWalk (pathWalk)
import System.Environment (getArgs)

main :: IO ()
main = do
  rawArgs <- getArgs
  let args = if rawArgs == [] then ["."] else rawArgs
  forM_ args $ \arg -> do
    pathWalk arg $ \root dirs files -> do
      putStrLn root
      putStrLn $ "  dirs: " ++ show dirs
      putStrLn $ "  files: " ++ show files
