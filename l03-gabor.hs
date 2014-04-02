module Main where

import CV.Image
import CV.Filters
import CV.ImageMath as IM hiding (div)
import CV.ColourUtils

import Filters
import Gabor

size = 7
center = getMaskCenter2D size
mask = g1

g = [g1,g2,g3,g4,g5,g6,g7,g8]

main = do
  img <- readFromFile "park.png"
  let
    (gimg1,gimg2) = filterGabor mask size center img
  saveImage "gabor.png" $ montage (2,2) 2 $
    [ stretchHistogram gimg1
    , stretchHistogram gimg2
    , stretchHistogram $ IM.sqrt $ (IM.add (IM.mul gimg1 gimg1) (IM.mul gimg2 gimg2))
    , stretchHistogram $ IM.atan2 gimg2 gimg1
    ]
