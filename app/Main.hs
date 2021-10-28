module Main where

import Data.Map (Map, empty, findWithDefault, insert)
import Data.Sort
import Lib
import System.Environment

parse :: [String] -> Map String Double
parse [] = empty
parse (a : s)
  | take 3 a `elem` ((<> "=") <$> ["lx", "ly", "lz", "nx", "ny", "nz"]) = insert (take 2 a) (read $ drop 3 a) $ parse s
  | take 2 a == "n=" = insert "n" (read $ drop 2 a) $ parse s
  | otherwise = parse s

main :: IO ()
main = do
  vars <- parse <$> getArgs
  let n = floor $ findWithDefault 3 "n" vars
      lx = findWithDefault 20 "lx" vars
      ly = findWithDefault lx "ly" vars
      lz = findWithDefault lx "lz" vars
      nx = findWithDefault 40 "nx" vars
      ny = findWithDefault nx "ny" vars
      nz = findWithDefault nx "nz" vars
      ff = fromIntegral . floor
      nx2 = ff $ nx / 2
      ny2 = ff $ ny / 2
      nz2 = ff $ nz / 2
      l = (lx / nx, ly / ny, lz / nz)
      d p = d3f n l p
      sqr (a, b, c) (i, j, k) = (i * a) ** 2 + (j * b) ** 2 + (k * c) ** 2
      check a b = (abs (x - y) / y) < 1e-14
        where
          x = fst a
          y = fst b
      squish [] = []
      squish [a] = [a]
      squish (a : b : s)
        | check a b = squish (b : s)
        | otherwise = a : squish (b : s)
      ss = squish $ sort [let p = (i, j, k) in (sqr l p, p) | i <- [- nx2 .. nx - nx2 -1], j <- [- ny2 .. ny - ny2 -1], k <- [- nz2 .. nz - nz2 -1]]
      m = d . snd <$> ss
      go [] = return ()
      go (a : s) = print a >> go s
  go m
