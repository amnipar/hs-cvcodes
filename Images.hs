module Images
( discGrayImage
, torusGrayImage
, squareGrayImage
, diamondGrayImage
, powImage
) where

import CV.Image
import CV.Pixelwise

import BasicUtils

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
