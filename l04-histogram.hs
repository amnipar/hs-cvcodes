module Main where

import CV.Image
import CV.Filters
import CV.ColourUtils
import CV.Operations

import BasicUtils
import Images
import Histogram
import DrawingUtils
import Thresholding
import Random
import Filters
import Fourier
import Gabor

import qualified Data.List as List
import qualified Data.Map as Map
import Data.Ord

import ReadArgs

-- plot width in pixels
width = 400
-- plot height in pixels
height = 300
-- plot margin
margin = 10

gSize = 7
gCenter = getMaskCenter2D gSize
gMask = g1

mSize = 3
mCenter = getMaskCenter2D(mSize)
mMask = listToMask2D (mSize,mSize) $
  [ -1, 0, 1,
    -1, 0, 1,
    -1, 0, 1 ]

main = do
  (nbins, inputImage, outputImage) <- readArgs
  img <- readFromFile inputImage
  let
    mimg = convolve2D mMask mCenter img
    (gre,gim) = filterGabor gMask gSize gCenter img
    (gamp,gpha) = dftToPolar2D (gim,gre)
    --ghist = accHistogram 100 $ getGaussianVector 10 100000
    mhist = accHistogram nbins $ getValues $ unitNormalize mimg
    ghist = accHistogram nbins $ getValues $ logNormalize gre
    hist = accHistogram nbins $ getValues $ gaussian (5,5) img
    (t,s) = tOtsu hist
  print (t,s)
  saveImage outputImage $ montage (3,1) 2 $
    [ --convGrayToColor $ threshold (1,0) t img
      plotHistogram black margin mhist $
        emptyColorImage (width,height) white
    , plotHistogram black margin hist $
        emptyColorImage (width,height) white
    , plotHistogram black margin ghist $
        emptyColorImage (width,height) white
    ]
