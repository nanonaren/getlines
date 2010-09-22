module Main
    (
    ) where

import System.IO
import Data.List (sort)
import System.Console.ParseArgs

arguments :: [Arg String]
arguments =
    [
     Arg "nosort" (Just 'n') (Just "nosort") Nothing
             "Does not sort the provided line numbers"
    ]

main = do
  ags <- parseArgsIO ArgsTrailing arguments
  let nosort = gotArg ags "nosort"
      inpfile = head $ argsRest ags

  --get file contents
  lns <- fmap lines $ readFile inpfile
  lineNos <- fmap (map read.words) $ hGetContents stdin

  mapM_ putStrLn $
        filterLines (if nosort then lineNos else sort lineNos) lns

filterLines ns ls = filterLines' ns (zip [1..] ls)
filterLines' [] _ = []
filterLines' (n:ns) lns@((i,l):ls)
    | i > n = filterLines' ns lns
    | i == n = l : filterLines' ns ls
    | otherwise = filterLines' (n:ns) $ dropWhile ((/=n).fst) lns
