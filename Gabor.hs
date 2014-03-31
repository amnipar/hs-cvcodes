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
, gabormask
) where

import CV.Image
import CV.Filters
import CV.ImageMath as IM hiding (div)
import CV.ImageMathOp
import CV.Matrix as M
import CV.ColourUtils
import CV.Pixelwise

i = fromIntegral

gabor :: Float -> Float -> Float -> Float -> Float -> (Int,Int) -> (Float,Float)
gabor lambda theta psi sigma gamma (x,y) =
  (norm * gaussian * rharmonic, norm * gaussian * iharmonic)
  where
    sigma_x = sigma
    sigma_y = sigma / gamma
    x' = (i x) * (cos theta) + (i y) * (sin theta)
    y' = -(i x) * (sin theta) + (i y) * (cos theta)
    norm = 1 / (2 * pi * sigma_x * sigma_y)
    gaussian = exp $ (-0.5) * (x'**2 / sigma_x**2 + y'**2 / sigma_y**2)
    rharmonic = cos $ (2 * pi * x') / lambda + psi
    iharmonic = sin $ (2 * pi * x') / lambda + psi

g1 = gabor 2.33 0        0 1 0.5
g2 = gabor 2.33 (pi/8)   0 1 0.5
g3 = gabor 2.33 (pi/4)   0 1 0.5
g4 = gabor 2.33 (3*pi/8) 0 1 0.5
g5 = gabor 2.33 (pi/2)   0 1 0.5
g6 = gabor 2.33 (5*pi/8) 0 1 0.5
g7 = gabor 2.33 (3*pi/4) 0 1 0.5
g8 = gabor 2.33 (7*pi/8) 0 1 0.5

{-
gi1 = gabor 30 0        0      17 1
gi2 = gabor 30 (pi/4)   0      17 1
gi3 = gabor 30 (pi/2)   0      17 1
gi4 = gabor 30 (3*pi/4) 0      17 1
gi5 = gabor 30 0        (pi/2) 17 1
gi6 = gabor 30 (pi/4)   (pi/2) 17 1
gi7 = gabor 30 (pi/2)   (pi/2) 17 1
gi8 = gabor 30 (3*pi/4) (pi/2) 17 1
-}

kernel :: ((Int,Int) -> Float) -> Int -> [Float]
kernel f r = [f (x,y) | y <- [-r..r] , x <- [-r..r] ]

gabormask :: (Int,Int) -> ((Int,Int) -> (Float,Float))
    -> (Image GrayScale D32, Image GrayScale D32)
gabormask (s,_) f = (imageFromFunction (s,s) f1, imageFromFunction (s,s) f2)
  where
    r = s `div` 2
    f1 (x,y) = fst $ f (x-r,y-r)
    f2 (x,y) = snd $ f (x-r,y-r)
{-
gmaskit7 :: Image GrayScale D32
gmaskit7 = montage (4,2) 4
  [ stretchHistogram $ imageFromFunction (7,7) (f g1)
  , stretchHistogram $ imageFromFunction (7,7) (f g2)
  , stretchHistogram $ imageFromFunction (7,7) (f g3)
  , stretchHistogram $ imageFromFunction (7,7) (f g4)
  , stretchHistogram $ imageFromFunction (7,7) (f g5)
  , stretchHistogram $ imageFromFunction (7,7) (f g6)
  , stretchHistogram $ imageFromFunction (7,7) (f g7)
  , stretchHistogram $ imageFromFunction (7,7) (f g8)
  ]
  where
    f op (x,y) = op (x-3,y-3)

gmaskit15 :: Image GrayScale D32
gmaskit15 = montage (4,2) 4
  [ stretchHistogram $ imageFromFunction (15,15) (f g1)
  , stretchHistogram $ imageFromFunction (15,15) (f g2)
  , stretchHistogram $ imageFromFunction (15,15) (f g3)
  , stretchHistogram $ imageFromFunction (15,15) (f g4)
  , stretchHistogram $ imageFromFunction (15,15) (f g5)
  , stretchHistogram $ imageFromFunction (15,15) (f g6)
  , stretchHistogram $ imageFromFunction (15,15) (f g7)
  , stretchHistogram $ imageFromFunction (15,15) (f g8)
  ]
  where
    f op (x,y) = op (x-7,y-7)

gaborit :: Image GrayScale D32 -> Image GrayScale D32
gaborit kuva = montage (4,2) 4
  [ stretchHistogram $ convolve2D (maski g1) keskipiste kuva
  , stretchHistogram $ convolve2D (maski g2) keskipiste kuva
  , stretchHistogram $ convolve2D (maski g3) keskipiste kuva
  , stretchHistogram $ convolve2D (maski g4) keskipiste kuva
  , stretchHistogram $ convolve2D (maski g5) keskipiste kuva
  , stretchHistogram $ convolve2D (maski g6) keskipiste kuva
  , stretchHistogram $ convolve2D (maski g7) keskipiste kuva
  , stretchHistogram $ convolve2D (maski g8) keskipiste kuva
  ]
  where
    keskipiste :: (Int,Int)
    keskipiste = (3,3)
    maski f = M.fromList (7,7) $ kernel f 3
    -}