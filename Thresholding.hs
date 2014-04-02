module Thresholding
( threshold
, tFromMeanDev
, convZeroOneToMinusPlus
, collectPoints
) where

import CV.Image
import CV.ImageMath
import CV.ImageMathOp
import CV.Pixelwise

import Images

threshold :: (Float,Float) -> Float -> Image GrayScale D32
    -> Image GrayScale D32
threshold m t image = mapImage (clamp m t) image
  where
    clamp (lower,higher) t v | v < t     = lower
                             | otherwise = higher

tFromMeanDev :: Float -> Float -> Image GrayScale D32 -> Float
tFromMeanDev k epsilon image = mu + k * sigma + epsilon
  where
    mu = average image
    sigma = stdDeviation image

-- converts an image in range (0,1) to image in range (-1,+1)
convZeroOneToMinusPlus :: Image GrayScale Float -> Image GrayScale Float
convZeroOneToMinusPlus image = (2 |* image) |- 1

-- | Collects all points from image that have a value higher than threshold
collectPoints :: Float -> Image GrayScale Float -> [((Int,Int),Float)]
collectPoints t image = filter (higherThan t) $ getPixels image
  where
    higherThan t (_,v) | v > t     = True
                       | otherwise = False
