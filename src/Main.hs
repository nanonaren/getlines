module Main
    (
    ) where

import System.Environment (getArgs)
import System.IO
import Data.List (sort)

main = do
  filename <- fmap (!!0) getArgs
  lns <- fmap lines $ readFile filename
  lineNos <- fmap (sort.map read.words) $ hGetContents stdin
  stuff lineNos (zip [1..] lns)

stuff [] _ = return ()
stuff (n:lineNos) ((i,l):lns)
          | i == n = putStrLn l >> stuff lineNos lns
          | i < n = stuff (n:lineNos) lns
          | otherwise = stuff lineNos ((i,l):lns)