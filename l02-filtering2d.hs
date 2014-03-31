module Main where

import CV.Image
import CV.Filters
import CV.Matrix as M
import Filters

-- mask size; odd values recommended, like 3,5,7
maskSize = 5 -- 3 for manualMask
-- mask centerpoint
maskCenter = getMaskCenter2D(maskSize)
-- options : average mask, gaussian mask, manual mask
mask = averageMask2D(maskSize)
--mask = gaussianMask2D(maskSize)
--mask = manualMask
-- manually defined convolution mask; remember to set maskSize to 3!
manualMask :: Matrix Float
manualMask = M.fromList (3,3) $
  [ -1, 0, 1,
    -1, 0, 1,
    -1, 0, 1 ]

main = do
  img <- readFromFile "park.png"
  saveImage "filtered-image.png" $ montage (1,2) 2 $
    [ img
    , convolve2D mask maskCenter img
    ]
