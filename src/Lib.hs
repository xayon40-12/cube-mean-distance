module Lib where

import GHC.Conc (par)
import GHC.Conc.Sync (pseq)

mean :: (Fractional a) => [a] -> a
mean = (/) <$> sum <*> fromIntegral . length

dy n a =
  [ sqrt ((x - x') ^ 2 + (y - y') ^ 2 + (z - z') ^ 2)
    | x <- v,
      x' <- v,
      y <- v,
      y' <- v,
      z <- v,
      z' <- v
  ]
  where
    v = [i * a / n | i <- [0 .. n -1]]

infixl 6 +|

(+|) :: (Num a) => a -> a -> a
a +| b = a `par` b `pseq` a + b

d3f' 0 (a, b, c) (x, y, z) (x', y', z') = sqrt (a ** 2 * (x - x') ** 2 + b ** 2 * (y - y') ** 2 + c ** 2 * (z - z') ** 2)
d3f' n size (x0', y0', z0') (x1', y1', z1')
  | x0' == x1' = 8 * (d0 +| dx) +| 4 * (dy +| dY +| dz +| dZ +| dxy +| dxz +| dxY +| dxZ) +| 2 * (dyz +| dYZ +| dzY +| dyZ +| dxyz +| dxyZ +| dxzY +| dxYZ)
  | y0' == y1' = 8 * (d0 +| dy) +| 4 * (dx +| dX +| dz +| dZ +| dxy +| dyz +| dyX +| dyZ) +| 2 * (dxz +| dXZ +| dzX +| dxZ +| dxyz +| dxyZ +| dyzX +| dyXZ)
  | z0' == z1' = 8 * (d0 +| dz) +| 4 * (dx +| dX +| dy +| dY +| dxz +| dyz +| dzX +| dzY) +| 2 * (dxy +| dXY +| dyX +| dxY +| dxyz +| dxzY +| dyzX +| dzXY)
  | x0' == x1' && y0' == y1' = 16 * dx +| 8 * (d0 +| dxy +| dxz +| dxZ) +| 4 * (dz +| dZ +| dxyz +| dxyZ)
  | x0' == x1' && z0' == z1' = 16 * dx +| 8 * (d0 +| dxz +| dxy +| dxY) +| 4 * (dy +| dY +| dxyz +| dxzY)
  | y0' == y1' && z0' == z1' = 16 * dy +| 8 * (d0 +| dyz +| dxy +| dyX) +| 4 * (dx +| dX +| dxyz +| dyzX)
  | x0' == x1' && y0' == y1' && z0' == z1' = 24 * (dx +| dxy) +| 8 * (d0 +| dxyz)
  | otherwise = 8 * d0 +| 4 * (dx +| dX +| dy +| dY +| dz +| dZ) +| 2 * (dxy +| dXY +| dyz +| dYZ +| dxz +| dXZ) +| 2 * (dxY +| dyX +| dxZ +| dzX +| dzY +| dyZ) +| (dxyz +| dXYZ +| dxyZ +| dzXY +| dyXZ +| dxzY +| dxYZ +| dyzX)
  where
    d0 = d3f' (n -1) size (x, y, z) (x', y', z')

    dx = d3f' (n -1) size (x + h, y, z) (x', y', z')
    dX = d3f' (n -1) size (x, y, z) (x' + h, y', z')
    dy = d3f' (n -1) size (x, y + h, z) (x', y', z')
    dY = d3f' (n -1) size (x, y, z) (x', y' + h, z')
    dz = d3f' (n -1) size (x, y, z + h) (x', y', z')
    dZ = d3f' (n -1) size (x, y, z) (x', y', z' + h)

    dxy = d3f' (n -1) size (x + h, y + h, z) (x', y', z')
    dXY = d3f' (n -1) size (x, y, z) (x' + h, y' + h, z')
    dyz = d3f' (n -1) size (x, y + h, z + h) (x', y', z')
    dYZ = d3f' (n -1) size (x, y, z) (x', y' + h, z' + h)
    dxz = d3f' (n -1) size (x + h, y, z + h) (x', y', z')
    dXZ = d3f' (n -1) size (x, y, z) (x' + h, y', z' + h)

    dxY = d3f' (n -1) size (x + h, y, z) (x', y' + h, z')
    dyX = d3f' (n -1) size (x, y + h, z) (x' + h, y', z')
    dxZ = d3f' (n -1) size (x + h, y, z) (x', y', z' + h)
    dzX = d3f' (n -1) size (x, y, z + h) (x' + h, y', z')
    dzY = d3f' (n -1) size (x, y, z + h) (x', y' + h, z')
    dyZ = d3f' (n -1) size (x, y + h, z) (x', y', z' + h)

    dxyz = d3f' (n -1) size (x + h, y + h, z + h) (x', y', z')
    dXYZ = d3f' (n -1) size (x, y, z) (x' + h, y' + h, z' + h)
    dxyZ = d3f' (n -1) size (x + h, y + h, z) (x', y', z' + h)
    dzXY = d3f' (n -1) size (x, y, z + h) (x' + h, y' + h, z')
    dyXZ = d3f' (n -1) size (x, y + h, z) (x' + h, y', z' + h)
    dxzY = d3f' (n -1) size (x + h, y, z + h) (x', y' + h, z')
    dxYZ = d3f' (n -1) size (x + h, y, z) (x', y' + h, z' + h)
    dyzX = d3f' (n -1) size (x, y + h, z + h) (x' + h, y', z')
    h = 0.5
    x = x0' * 0.5
    y = y0' * 0.5
    z = z0' * 0.5
    x' = x1' * 0.5
    y' = y1' * 0.5
    z' = z1' * 0.5

d3f :: Int -> (Double, Double, Double) -> (Double, Double, Double) -> Double
d3f n size (u, v, w) = d3f' n size (0, 0, 0) (abs u * n2, abs v * n2, abs w * n2) / n2 ** 6
  where
    n2 = fromIntegral $ 2 ^ n
