module Main where

import CV.Image
import CV.ColourUtils
import Gabor

g = [g1,g2,g3,g4,g5,g6,g7,g8]

main = saveImage "gabor.png" $ montage (4,4) 2 $
  concat [map (stretchHistogram.fst) gs, map (stretchHistogram.snd) gs]
  where
    gs = map (gabormask (7,7)) g
