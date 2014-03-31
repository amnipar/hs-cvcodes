module Main where

import CV.Image
import CV.Filters
import CV.Matrix

-- options : filterManual, smoothAvg s, smoothGaussian s
--convolve = filterManual
convolve = smoothAvg 5
--convolve = smoothGaussian 7

filterManual = convolve2D mask cp
smoothAvg s = convolve2D (createAvgMask s) (getCenterPoint s)
smoothGaussian s = convolve2D (createGaussianMask s) (getCenterPoint s)

-- centerpoint of manual mask
cp :: (Int,Int)
cp = (1,1)
-- manually defined convolution mask
mask :: Matrix D32
mask = CVLang.fromList (3,3) $
  [ -1, 0, 1,
  -1, 0, 1,
  -1, 0, 1 ]

main = do
  img <- readImage "nut.png"
  displayImage "Alkuperäinen" $ img
  displayImage "Suodatettu"  $ grayToRGB $ convolve $ rgbToGray img

avg :: Float -> (Int,Int) -> Float
avg  v (x,y) = v
