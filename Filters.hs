module Filters
( convolve1D
, averageMask1D
, gaussianMask1D
, createMask1D
) where

import Data.List
import BasicUtils

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

g :: Float -> Int -> Float
g s x = (1 / (sqrt $ 2 * pi * s**2)) * exp (-((iToF x)**2)/(2 * s**2))

kernel1D :: (Int -> Float) -> Int -> [Float]
kernel1D f r = [f x | x <- [-r..r]]

createMask1D :: (Float -> Int -> Float) -> Int -> [Float]
createMask1D f l = kernel1D (f s) r
  where
    r | even l = (l `div` 2) - 1
      | otherwise = (l `div` 2)
    s = (iToF l) / 6

averageMask1D :: Int -> [Float]
averageMask1D n = replicate n (1 / (iToF n))

gaussianMask1D :: Int -> [Float]
gaussianMask1D n = createMask1D g n
