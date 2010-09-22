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
  mapM_ putStrLn $ filterLines lineNos lns

filterLines ns ls = filterLines' ns (zip [1..] ls)
filterLines' [] _ = []
filterLines' (n:ns) lns@((i,l):ls)
    | i > n = filterLines' ns lns
    | i == n = l : filterLines' ns ls
    | otherwise = filterLines' (n:ns) $ dropWhile ((/=n).fst) lns
