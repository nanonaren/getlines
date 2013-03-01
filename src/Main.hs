{-# LANGUAGE DeriveDataTypeable #-}
module Main
    (
      main
    ) where

import System.IO
import Data.List (sort)
import qualified Data.ByteString.Lazy.Char8 as C
import System.Console.CmdArgs
import System.Directory (doesFileExist)

data Options = Options
    {
      noSort :: Bool,
      file :: FilePath
    } deriving (Show,Data,Typeable)

opts = Options
       {
         noSort = def &= help "Assume given line numbers is sorted.",
         file = def &= argPos 0 &= typFile
       } &= summary "getlines v0.1, (C) Naren Sundar 2010"
         &= program "getlines"
         &= details ["EXAMPLE: get lines 37 and 43 from file.",
                     "$ echo \"37 43\" | getlines file","","",
                     "http://github.com/nanonaren/getlines"]

main = do
  options <- cmdArgs opts
  --check file exists
  exists <- doesFileExist (file options)
  if not exists then fail "Cannot open input file" else return ()

  -- read lines
  lns <- C.lines `fmap` C.readFile (file options)
  lineNos <- fmap (map read.words) $ hGetContents stdin

  -- filter lines
  mapM_ C.putStrLn $
        filterLines (if (noSort options) then lineNos else sort lineNos) lns

filterLines ns ls = filterLines' ns (zip [1..] ls)
filterLines' (n:ns) lns@((i,l):ls)
    | i > n = filterLines' ns lns
    | i == n = l : filterLines' ns ls
    | otherwise = filterLines' (n:ns) ls
filterLines' [] _ = []
filterLines' _ [] = []