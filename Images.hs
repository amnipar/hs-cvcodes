{-# LANGUAGE FlexibleContexts #-}
module Images
( emptyGrayImage
, emptyColorImage
, convGrayToColor
, convColorToGray
, discGrayImage
, torusGrayImage
, squareGrayImage
, diamondGrayImage
, powImage
, getPixels
, getValues
, naiveUpscale
, resizeImage
, resizeImageFaithful
, scaleImage
, zeroNormalize
) where

import CV.Image
import qualified CV.ImageMath as IM
import CV.ImageMathOp
import CV.Pixelwise
import CV.Transforms

import BasicUtils

-- | Creates an empty grayscale image of the specific shade. Can be used as a
--   starting point for drawing operations.
emptyGrayImage :: (Int,Int) -> D32 -> Image GrayScale Float
emptyGrayImage (w,h) color = imageFromFunction (w,h) (const color)

-- | Creates an empty color image of the specific shade. Can be used as a
--   starting point for drawing operations.
emptyColorImage :: (Int,Int) -> (Float,Float,Float) -> Image RGB Float
emptyColorImage (w,h) color = imageFromFunction (w,h) (const color)

convGrayToColor :: Image GrayScale Float -> Image RGB Float
convGrayToColor = grayToRGB

convColorToGray :: Image RGB Float -> Image GrayScale Float
convColorToGray = rgbToGray

-- draws an image of size s, containing a disc of radius r, of color fc, on
-- background of color bc
discGrayImage :: Int -> Int -> Float -> Float -> Image GrayScale Float
discGrayImage s r fc bc = imageFromFunction (s,s) f
  where
    c = (iToF s) / 2
    f (x,y) | d < (iToF r)^2 = fc
            | otherwise      = bc
            where
              d = ((iToF x)-c+0.5)^2+((iToF y)-c+0.5)^2

-- draws an image of size s, containing a torus with inner radius r1 and outer
-- radius r2, of color fc, on background of color bc
torusGrayImage :: Int -> Int -> Int -> Float -> Float -> Image GrayScale Float
torusGrayImage s r1 r2 fc bc = imageFromFunction (s,s) f
  where
    c = (iToF s) / 2
    f (x,y) | d < (iToF r2)^2 && d > (iToF r1)^2 = fc
            | otherwise                          = bc
            where
              d = ((iToF x)-c+0.5)^2+((iToF y)-c+0.5)^2

-- draws an image of size s, containing a square with radius r, of color fc, on
-- background of color bc
squareGrayImage :: Int -> Int -> Float -> Float -> Image GrayScale Float
squareGrayImage s r fc bc = imageFromFunction (s,s) f
  where
    c = (iToF s) / 2
    f (x,y) | abs ((iToF x)-c+0.5) < (iToF r) &&
              abs ((iToF y)-c+0.5) < (iToF r)    = fc
            | otherwise                          = bc

-- draws an image of size s, containing a diamond with radius r, of color fc, on
-- background of color bc
diamondGrayImage :: Int -> Int -> Float -> Float -> Image GrayScale Float
diamondGrayImage s r fc bc = imageFromFunction (s,s) f
  where
    c = (iToF s) / 2
    f (x,y) | (abs $ (iToF x)-c+0.5)+(abs $ (iToF y)-c+0.5) < 2*(iToF r) = fc
            | otherwise                                                  = bc

-- | Generates a dft amplitude image corresponding to 1/f power distribution
powImage :: (Int,Int) -> Image GrayScale D32
powImage (w,h) = imageFromFunction (w,h) f
  where
    w2 = w `div` 2
    h2 = h `div` 2
    f (x,y) | x == 0  && y == 0  = 1
            | x <= w2 && y <= h2 = 1 / (sqrt $ (iToF x)^2 + (iToF y)^2)
            | x <= w2 && y >  h2 = 1 / (sqrt $ (iToF x)^2 + (iToF $ h-y)^2)
            | x >  w2 && y <= h2 = 1 / (sqrt $ (iToF $ w-x)^2 + (iToF y)^2)
            | otherwise          = 1 / (sqrt $ (iToF $ w-x)^2 + (iToF $ h-y)^2)

-- | Turns an image into a list of point-value pairs.
getPixels :: Image GrayScale Float -> [((Int,Int),Float)]
getPixels image =
  concat $ [[((x,y), getPixel (x,y) image)
            | y<-[0..h-1]] | x <- [0..w-1]]
  where
    (w,h) = getSize image

-- | Turns an image into a list of values.
getValues :: Image GrayScale Float -> [Float]
getValues image = [getPixel (x,y) image | x <- [0..w-1], y <- [0..h-1]]
  where (w,h) = getSize image

-- | Creates a naively upscaled version of the image by replicating the pixels
--   s times in both directions.
naiveUpscale :: Int -> Image GrayScale D32 -> Image GrayScale D32
naiveUpscale factor image = imageFromFunction (nw,nh) f
  where
    (w,h) = getSize image
    nw = factor*w
    nh = factor*h
    f (x,y) = getPixel (x',y') image
      where
        x' = x `div` factor
        y' = y `div` factor

-- | Forces the image to given size, not considering the aspect ratio.
resizeImage :: (CreateImage (Image c Float)) =>
  (Int,Int) -> Image c Float -> Image c Float
resizeImage size image = scaleToSize Cubic False size image

-- | Resize the image to given size, preserving the aspect ratio.
resizeImageFaithful :: (CreateImage (Image c Float)) =>
  (Int,Int) -> Image c Float -> Image c Float
resizeImageFaithful size image = scaleToSize Cubic True size image

scaleImage :: (CreateImage (Image c Float)) =>
  (Float,Float) -> Image c Float -> Image c Float
scaleImage ratio image = scale Cubic ratio image

zeroNormalize image = norm |* (iext |+ image)
  where
    (imin,imax) = IM.findMinMax image
    iext = max (abs imin) imax
    norm = 1 / (2 * iext)
