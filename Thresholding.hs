module Thresholding
( threshold
, quantizeAngle4
, tFromMeanDev
, tOtsu
, convZeroOneToMinusPlus
, collectPoints
, relativeThresholdPoints
, relativeThresholdPoints2
) where

import CV.Image
import CV.ImageMath hiding (sum)
import CV.ImageMathOp
import CV.Pixelwise

import Images

import Data.List
import Data.Ord

-- | Thresholds an image into a two-value image using the given (low,high) and
--   threshold values.
threshold :: (Float,Float) -> Float -> Image GrayScale Float
    -> Image GrayScale Float
threshold m t image = mapImage (clamp m t) image
  where
    clamp (lower,higher) t v | v < t     = lower
                             | otherwise = higher

pi_3_4 = 3 * pi / 4
pi_1_2 = pi / 2
pi_1_4 = pi / 4

quantizeAngle4 :: Image GrayScale Float -> Image GrayScale Float
quantizeAngle4 image = mapImage q4 image
  where
    q4 v | v > pi_3_4  = 0
         | v > pi_1_4  = 1
         | v > -pi_1_4 = 2
         | v > -pi_3_4 = 3
         | otherwise   = 0

-- | Finds the threshold value using mean-dev method
tFromMeanDev :: Float -> Float -> Image GrayScale D32 -> Float
tFromMeanDev k epsilon image = mu + k * sigma + epsilon
  where
    mu = average image
    sigma = stdDeviation image

-- | Finds the threshold value using Otsu's method on given image histogram
tOtsu :: [(Float,Float)] -> Float
tOtsu bins = t
  where
  t = combineBest $ reverse $
      sortBy (comparing snd) $ map (sigma_b bins) [1..n-1]
  n = length bins
  combineBest ((t1,_):(t2,_):bs) = (t1+t2)/2
  omega bs = sum $ map snd bs
  mu bs = (sum $ map (\(x,p) -> x*p) bs) / omega bs
  sigma_b bs i = (t,(omega bs1)*(omega bs2)*((mu bs1)-(mu bs2))**2)
    where
      t = fst $ last bs1
      (bs1,bs2) = splitAt i bs

-- | Converts an image in range (0,1) to image in range (-1,+1)
convZeroOneToMinusPlus :: Image GrayScale Float -> Image GrayScale Float
convZeroOneToMinusPlus image = (2 |* image) |- 1

-- | Collects all points from image that have a value higher than threshold
collectPoints :: Float -> Image GrayScale Float -> [((Int,Int),Float)]
collectPoints t image = filter (higherThan t) $ getPixels image
  where
    higherThan t (_,v) | v > t     = True
                       | otherwise = False

-- | Filter out points with values that are weaker than factor * maximum value
relativeThresholdPoints :: Float -> [((Int,Int),Float)] -> [((Int,Int),Float)]
relativeThresholdPoints factor cs = filter ((>t) . snd) cs
  where
    m = maximum $ map snd cs
    t = factor * m

-- Filter out points with pairs of values that are weaker than factor * maximum
-- of both values
relativeThresholdPoints2 :: Float -> [((Int,Int),(Float,Float))]
    -> [((Int,Int),(Float,Float))]
relativeThresholdPoints2 factor cs =
  filter (\(_,(v1,v2)) -> (v1 > t1) && (v2 > t2)) cs
  where
    v1 (_,(v,_)) = v
    v2 (_,(_,v)) = v
    m1 = maximum $ map v1 cs
    m2 = maximum $ map v2 cs
    t1 = factor * m1
    t2 = factor * m2
