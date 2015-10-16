module Main where

import Data.List (intercalate)
import System.Directory.PathWalk (pathWalk)

main :: IO ()
main = do
    pathWalk "./foo" $ \x y z ->
        putStrLn $ intercalate " " [(show x), (show y), (show z)]
