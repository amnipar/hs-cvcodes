module Gabor
( gabor
, g1
, g2
, g3
, g4
, g5
, g6
, g7
, g8
, gaborMask
, filterGabor
, gaborImage
) where

import CV.Image
import CV.Filters
import CV.Matrix as M
import CV.Pixelwise

import BasicUtils
import Filters

gabor :: Float -> Float -> Float -> Float -> Float -> (Int,Int) -> (Float,Float)
gabor lambda theta psi sigma gamma (x,y) =
  (norm * gaussian * rharmonic, norm * gaussian * iharmonic)
  where
    sigma_x = sigma
    sigma_y = sigma / gamma
    x' =  (iToF x) * (cos theta) + (iToF y) * (sin theta)
    y' = -(iToF x) * (sin theta) + (iToF y) * (cos theta)
    norm = 1 / (2 * pi * sigma_x * sigma_y)
    gaussian = exp $ (-0.5) * (x'**2 / sigma_x**2 + y'**2 / sigma_y**2)
    rharmonic = cos $ (2 * pi * x') / lambda + psi
    iharmonic = sin $ (2 * pi * x') / lambda + psi

-- TODO: parameterize by size/scale
g1 = gabor 2.33 0        0 1 0.5
g2 = gabor 2.33 (pi/8)   0 1 0.5
g3 = gabor 2.33 (pi/4)   0 1 0.5
g4 = gabor 2.33 (3*pi/8) 0 1 0.5
g5 = gabor 2.33 (pi/2)   0 1 0.5
g6 = gabor 2.33 (5*pi/8) 0 1 0.5
g7 = gabor 2.33 (3*pi/4) 0 1 0.5
g8 = gabor 2.33 (7*pi/8) 0 1 0.5

gaborMask :: ((Int,Int) -> (Float,Float)) -> Int -> (Matrix D32, Matrix D32)
gaborMask g s = (createMask2D (fst.g) s, createMask2D (snd.g) s)

filterGabor :: ((Int,Int) -> (Float,Float)) -> Int -> (Int,Int)
    -> Image GrayScale Float -> (Image GrayScale Float, Image GrayScale Float)
filterGabor g s cp img = (convolve2D imask cp img, convolve2D imask cp img)
  where
    (rmask,imask) = gaborMask g s

gaborImage :: (Int,Int) -> ((Int,Int) -> (Float,Float))
    -> (Image GrayScale D32, Image GrayScale D32)
gaborImage (s,_) f = (imageFromFunction (s,s) f1, imageFromFunction (s,s) f2)
  where
    r = s `div` 2
    f1 (x,y) = fst $ f (x-r,y-r)
    f2 (x,y) = snd $ f (x-r,y-r)
