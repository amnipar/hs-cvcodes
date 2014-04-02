module Main where

import CV.Image
import CV.Filters
import CV.ImageMath as IM hiding (div)
import CV.ColourUtils
import CV.Operations

import Filters
import Fourier
import Gabor

size = 7
center = getMaskCenter2D size
mask = g1

g = [g1,g2,g3,g4,g5,g6,g7,g8]

main = do
  img <- readFromFile "park.png"
  let
    (gre,gim) = filterGabor mask size center img
    (gamp,gpha) = dftToPolar2D (gre, gim)
  saveImage "gabor.png" $ montage (2,2) 2 $
    [ logNormalize gre
    , logNormalize gim
    , stretchHistogram gamp
    --IM.sqrt $ (IM.add (IM.mul gimg1 gimg1) (IM.mul gimg2 gimg2))
    , unitNormalize gpha
    -- $ IM.atan2 gimg2 gimg1
    ]
