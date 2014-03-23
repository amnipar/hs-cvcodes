module Main where

import CV.Image
import CV.Pixelwise
import CV.DFT
import CV.Operations
import CV.ImageMath as IM hiding (sqrt,div)
import CV.ImageMathOp

import DrawingUtils
import IOUtils

fi = fromIntegral

--cmp = undefined :: Tag Complex

--dftMerge :: (Image GrayScale D32, Image GrayScale D32) -> Image Complex D32
--dftMerge (re,im) =
--  composeMultichannelImage (Just re) (Just im) Nothing Nothing cmp

powImg :: (Int,Int) -> Image GrayScale D32
powImg (w,h) = imageFromFunction (w,h) f
  where
    w2 = w `div` 2
    h2 = h `div` 2
    f (x,y) | x == 0 && y == 0  = 1
            | x < w2 && y < h2  = 1 / (sqrt $ (fi x)^2 + (fi y)^2)
            | x < w2 && y >= h2 = 1 / (sqrt $ (fi x)^2 + (fi $ h-y)^2)
            | x >= w2 && y < h2 = 1 / (sqrt $ (fi $ w-x)^2 + (fi y)^2)
            | otherwise         = 1 / (sqrt $ (fi $ w-x)^2 + (fi $ h-y)^2)

combineMag :: Image GrayScale D32 -> Image GrayScale D32
    -> Image GrayScale D32
combineMag img1 img2 = imageFromFunction (w,h) (f img1 img2)
  where
    (w,h) = getSize img1
    w2 = w `div` 2
    w4 = fi $ w `div` 3
    h2 = h `div` 2
    h4 = fi $ h `div` 3
    f img1 img2 (x,y)
      | x <  w2 && y <  h2 && (sqrt $ (fi x)^2 + (fi y)^2) < h4 = v1
      | x <  w2 && y >= h2 && (sqrt $ (fi x)^2 + (fi $ h-y)^2) < h4 = v1
      | x >= w2 && y <  h2 && (sqrt $ (fi $ w-x)^2 + (fi y)^2) < h4 = v1
      | x >= w2 && y >= h2 && (sqrt $ (fi $ w-x)^2 + (fi $ h-y)^2) < h4 = v1
      | otherwise = v2
      where
        v1 = getPixel (x,y) img1
        v2 = getPixel (x,y) img2

main = do
  park <-readGrayImage "park.jpg"
  boat <- readGrayImage "boat.jpg"
  let
    pdft = dft park
    (pre,pim) = complexSplit pdft
    bdft = dft boat
    (bre,bim) = complexSplit bdft
    (pamp,ppha) = cartToPolar (pim,pre)
    (pw,ph) = getSize pamp
    pclear = powImg (pw,ph) -- emptyGrayImage (pw,ph) 1
    (bamp,bpha) = cartToPolar (bim,bre)
    (bw,bh) = getSize bamp
    bclear = powImg (bw,bh) -- emptyGrayImage (bw,bh) 1
  saveImage "dft.png" $ montage (2,4) 2
    [ logNormalize $ combineMag pamp bamp --pamp
    , unitNormalize ppha
    , logNormalize $ combineMag bamp pamp --bamp
    , unitNormalize bpha
    , unitNormalize $ idft $ compose $
        polarToCart ((combineMag pamp bamp),(combineMag ppha bpha))
    , unitNormalize $ idft $ compose $
        polarToCart ((combineMag bamp pamp),(combineMag bpha ppha))
    ]
