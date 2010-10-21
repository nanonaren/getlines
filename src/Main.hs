module Main
    (
    ) where

import System.IO
import Data.List (sort)
import System.Console.ParseArgs
import System.Directory (doesFileExist)

arguments :: [Arg String]
arguments =
    [
     Arg "nosort" (Just 'n') (Just "nosort") Nothing
             "Does not sort the provided line numbers"
    ]

main = do
  ags <- parseArgsIO ArgsTrailing arguments
  let nosort = gotArg ags "nosort"
      inpfile = let xs = argsRest ags in if null xs then "" else head xs

  --check file exists
  exists <- doesFileExist inpfile
  if not exists then fail "Cannot open input file" else return ()

  -- read lines
  lns <- fmap lines $ readFile inpfile
  lineNos <- fmap (map read.words) $ hGetContents stdin

  -- filter lines
  mapM_ putStrLn $
        filterLines (if nosort then lineNos else sort lineNos) lns

filterLines ns ls = filterLines' ns (zip [1..] ls)
filterLines' [] _ = []
filterLines' (n:ns) lns@((i,l):ls)
    | i > n = filterLines' ns lns
    | i == n = l : filterLines' ns ls
    | otherwise = filterLines' (n:ns) $ dropWhile ((/=n).fst) lns
