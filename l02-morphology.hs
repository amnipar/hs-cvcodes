module Main where

import CV.Image
import qualified CV.ImageMath as IM
import CV.ImageMathOp

import DrawingUtils
import Thresholding
import Morphology

import ReadArgs

main = do
  (inputImage, t, n, outputImage) <- readArgs
  img <- readFromFile inputImage
  let
    (w,h) = getSize img
    thresholded = threshold (1,0) t img
    dilated = dilate crossMask n thresholded
    eroded = erode squareMask n thresholded
  saveImage outputImage $ montage (2,2) 2
    [ img
    , thresholded
    , dilated
    , eroded
    ]
