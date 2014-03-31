module Filters
( convolve1D
, averageMask1D
, gaussianMask1D
, createMask1D
) where

import CV.Matrix as M

import Data.List
import BasicUtils
import Gaussian

-- | Calculates naively the 1D convolution of two signals g and f.
convolve1D :: [Float] -> Int -> [(Float,Float)] -> [(Float,Float)]
convolve1D g cp f = zip xs $ take (length xs) gy
  where
    xs = map fst f
    ys = map snd f
    -- need to pad the signal from both ends to make the kernel fit
    ys' = (replicate cp (head ys)) ++ ys ++ (replicate (l-cp-1) (last ys))
    gy = map (sum . zipWith (*) (reverse g)) (init $ tails ys')
    l = length g

kernel1D :: (Int -> Float) -> Int -> [Float]
kernel1D f r = [f x | x <- [-r..r]]

kernel2D :: ((Int,Int) -> Float) -> Int -> [Float]
kernel2D f r = [f (x,y) | y <- [-r..r] , x <- [-r..r] ]

-- given mask size, calculate radius
getMaskRadius :: Int -> Int
getMaskRadius s
  | even s = (s `div` 2) - 1
  | otherwise = (s `div` 2)

getMaskCenter1D :: Int -> Int
getMaskCenter1D s = getMaskRadius s

-- given mask size, calculate centerpoint
getMaskCenter2D :: Int -> (Int,Int)
getMaskCenter2D s = (r,r)
  where
    r = getMaskRadius s

createMask1D :: (Float -> Int -> Float) -> Int -> [Float]
createMask1D f s = kernel1D (f s) r
  where
    r = getMaskRadius l

createMask2D :: (Float -> Int -> Float) -> Int -> Matrix Float
createMask2D f s

averageMask1D :: Int -> [Float]
averageMask1D n = replicate n (1 / (iToF n))

gaussianMask1D :: Int -> [Float]
gaussianMask1D n = createMask1D g n
  where
    s = (iToF l) / 6

-- given mask size, calculate kernel matrix
gaussianMask2D :: Int -> Matrix Float
gaussianMask2D s =
  M.fromList (d,d) $ kernel2D (gaussian2D sigma) r
  where
        r = getRadius s
        d = 2 * r + 1
        -- must fit 3 sigma in (floating point) radius = d / 2
        sigma = (iToF d) / 6

averageMask2D :: Int -> Matrix Float
averageMask2D s = M.fromList (s,s) $ replicate n (1 / (iToF n))
  where
    n = s*s
