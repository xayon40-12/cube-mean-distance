module Main where

import Lib

main :: IO ()
main = do
  let n = 6
      u = 0
      v = 0
      w = 0
  putStrLn $ "2**n              : " <> show (2 ** n)
  putStrLn $ "2**-n             : " <> show (2 ** (- n))
  putStrLn $ "sqrt(u^2+v^2+w^2) : " <> show (sqrt (u ^ 2 + v ^ 2 + w ^ 2))
  putStrLn $ "numerics          : " <> show (d3f n (1, 1, 1) (u, v, w))
