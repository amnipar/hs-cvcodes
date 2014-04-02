module Filters
( getMaskRadius
, convolve1D
, getMaskCenter1D
, kernel1D
, createMask1D
, averageMask1D
, gaussianMask1D
, getMaskCenter2D
, kernel2D
, createMask2D
, listToMask2D
, averageMask2D
, gaussianMask2D
) where

import CV.Matrix as M

import Data.List

import BasicUtils
import Gaussian

-- | given the mask size, calculate radius.
getMaskRadius :: Int -> Int
getMaskRadius s
  | even s = (s `div` 2) - 1
  | otherwise = (s `div` 2)

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

-- | Given the mask size, calculate centerpoint coordinates.
getMaskCenter1D :: Int -> Int
getMaskCenter1D s = getMaskRadius s

-- | Given a function, centerpoint and size, generates a list of 1D kernel
--   values by sampling the function in mask points.
kernel1D :: (Int -> Float) -> Int -> Int -> [Float]
kernel1D f cx s = [f x | x <- [x1..x2]]
  where
    x1 = -cx
    x2 = s - cx - 1

-- | Create a symmetric 1D mask by sampling a function in mask points. Origin
--   will be in the mask centerpoint. For even-sized masks, the centerpoint
--   will be in the point to the right and below from the actual center. If this
--   is not desired, use the kernel1D function manually.
createMask1D :: (Int -> Float) -> Int -> [Float]
createMask1D f s = kernel1D f (getMaskCenter1D s) s

-- | Given the mask size, create a 1D averaging mask.
averageMask1D :: Int -> [Float]
averageMask1D s = replicate s (1 / (iToF s))

-- | Given the mask size, create a 1D Gaussian kernel mask.
gaussianMask1D :: Int -> [Float]
gaussianMask1D s = createMask1D (gaussian1D sigma) s
  where
    -- must fit 3 sigma in (floating point) radius = s / 2
    sigma = (iToF s) / 6

-- | Given the mask size, calculate centerpoint coordinates.
getMaskCenter2D :: Int -> (Int,Int)
getMaskCenter2D s = (r,r)
  where
    r = getMaskRadius s

-- | Given a function, centerpoint and size, generates a list of 2D kernel
--   values by sampling the function in mask points.
kernel2D :: ((Int,Int) -> Float) -> (Int,Int) -> (Int,Int) -> [Float]
kernel2D f (cx,cy) (sx,sy) = [f (x,y) | y <- [y1..y2] , x <- [x1..x2]]
  where
    x1 = -cx
    x2 = sx - cx - 1
    y1 = -cy
    y2 = sy - cy - 1

-- | Create a symmetric 2D mask by sampling a function in mask points. Origin
--   will be in the mask centerpoint. For even-sized masks, the centerpoint will
--   be in the point to the right and below from the actual center. If this is
--   not desired, use the kernel2D function manually.
createMask2D :: ((Int,Int) -> Float) -> Int -> Matrix Float
createMask2D f s = M.fromList (s,s) $ kernel2D f (getMaskCenter2D s) (s,s)
  where
    r = getMaskRadius s

-- | Create a mask from a manually defined list. Transpose is needed, so that
--   the intuitive row-major ordering of elements is reflected in the
--   convolution result.
listToMask2D :: (Int,Int) -> [Float] -> Matrix Float
listToMask2D s fs = M.transpose $ M.fromList s fs

-- | Given the mask size, create a 2D averaging mask.
averageMask2D :: Int -> Matrix Float
averageMask2D s = M.fromList (s,s) $ replicate n (1 / (iToF n))
  where
    n = s*s

-- | Given the mask size, create a 2D Gaussian kernel mask.
gaussianMask2D :: Int -> Matrix Float
gaussianMask2D s = createMask2D (gaussian2D sigma) s
  where
    -- must fit 3 sigma in (floating point) radius = d / 2
    sigma = (iToF s) / 6
