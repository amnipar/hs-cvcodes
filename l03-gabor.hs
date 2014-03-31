module Main where

import CV.Image
import CV.Filters
import CV.ImageMath as IM hiding (div)
import CV.Matrix as M
import CV.ColourUtils
--import Gabor

gaborMask = g1

main = do
  img <- readFromFile "park.png"
  let
    (gimg1,gimg2) = gaborFilter gaborMask img
  saveImage "gabor.png" $ montage (2,2) 2 $
    [ stretchHistogram gimg1
    , stretchHistogram gimg2
    , stretchHistogram $ IM.sqrt $ (IM.add (IM.mul gimg1 gimg1) (IM.mul gimg2 gimg2))
    , stretchHistogram $ IM.atan2 gimg2 gimg1
    ]
    --montage (4,4) 2 $
      --concat [map (stretchHistogram.fst) gs, map (stretchHistogram.snd) gs]
      --where
            --gs = map (gabormask (7,7)) g

cp :: (Int,Int)
cp = (3,3)

gaborFilter g img = (convolve2D imask cp img, convolve2D imask cp img)
  where
        (rmask,imask) = createGaborMask g

kernel :: ((Int,Int) -> Float) -> Int -> [Float]
kernel f r = [f (x,y) | y <- [-r..r] , x <- [-r..r] ]


createGaborMask :: ((Int,Int) -> (Float,Float)) -> (Matrix D32, Matrix D32)
createGaborMask g =
  (M.fromList (7,7) $ kernel (fst.g) 3,
   M.fromList (7,7) $ kernel (snd.g) 3)

fi = fromIntegral

g = [g1,g2,g3,g4,g5,g6,g7,g8]

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
