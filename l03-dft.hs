module Main where

import CV.Image
import CV.Pixelwise
import CV.DFT
import CV.Operations
import CV.ImageMath as IM hiding (sqrt,div)
import CV.ImageMathOp

import BasicUtils
import IOUtils
import DrawingUtils
import Images

--cmp = undefined :: Tag Complex

--dftMerge :: (Image GrayScale D32, Image GrayScale D32) -> Image Complex D32
--dftMerge (re,im) =
--  composeMultichannelImage (Just re) (Just im) Nothing Nothing cmp

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
    pclear = powImage (pw,ph)
    (bamp,bpha) = cartToPolar (bim,bre)
    (bw,bh) = getSize bamp
    bclear = powImage (bw,bh)
  saveImage "dft.png" $ montage (2,3) 2
    [ logNormalize $ pamp
    , unitNormalize ppha
    , logNormalize $ bamp
    , unitNormalize bpha
    , unitNormalize $ idft $ compose $
        polarToCart (pclear,ppha)
    , unitNormalize $ idft $ compose $
        polarToCart (bclear,bpha)
    ]
