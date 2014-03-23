module Thresholding
( threshold
, tFromMeanDev
) where

import CV.Image
import CV.ImageMath
import CV.Pixelwise

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

