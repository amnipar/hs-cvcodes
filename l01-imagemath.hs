module Main where

import CV.Image
import CV.ImageOp
import CV.ImageMath as IM
import CV.ImageMathOp
import CV.Pixelwise
import CV.ColourUtils
import CV.Operations

import IOUtils

shift :: (Int,Int) -> Image GrayScale D32 -> Image GrayScale D32
shift (sx,sy) image = imageFromFunction (w,h) f
  where
    (w,h) = getSize image
    f (x,y) | x < sx || y < sy = getPixel (x,y) image
            | otherwise        = getPixel (x-sx,y-sy) image

diff :: Image GrayScale D32 -> Image GrayScale D32 -> Image GrayScale D32
diff a b = IM.sqrt $ (a #- b) #* (a #- b)

main = do
  img1 <- readGrayImage "rect1.png"
  img2 <- readGrayImage "rect2.png"
  saveImage "result.png" $
    montage (2,4) 4 $
      [ img1
      , img2
      , unitNormalize $ IM.log img1
      , unitNormalize $ IM.log img2
      , unitNormalize $ IM.exp $ 1 |+ img1
      , unitNormalize $ IM.exp $ 1 |+ img2
      , unitNormalize $ diff img1 $ shift (1,1) img1
      , unitNormalize $ img1 #- shift (1,1) img1
      ]
