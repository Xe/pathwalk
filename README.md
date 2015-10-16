pathwalk
========

Some really simple path walking tools and utilities for Haskell.

```haskell
module Main where

import Data.List (intercalate)
import System.Directory.PathWalk (pathWalk)

main :: IO ()
main =
    pathWalk "./foo" $ \x y z ->
        putStrLn $ intercalate " " [(show x), (show y), (show z)]
```
