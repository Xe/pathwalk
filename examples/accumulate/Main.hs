module Main (main) where

import Control.Monad (forM_)
import Data.Monoid (Sum(..))
import System.Directory.PathWalk (pathWalkAccumulate)
import System.Environment (getArgs)

main :: IO ()
main = do
    rawArgs <- getArgs
    let args = if rawArgs == [] then ["."] else rawArgs
    forM_ args $ \arg -> do
        (Sum total) <- pathWalkAccumulate arg $ \root _dirs files -> do
            putStrLn $ "files in " ++ root ++ ": " ++ show (length files)
            return $ Sum (length files)
        putStrLn $ "Total: " ++ show total
