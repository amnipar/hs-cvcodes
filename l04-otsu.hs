module Main where

import CV.Image
import CV.Filters

import BasicUtils
import Images
import Histogram
import Thresholding

import ReadArgs

main = do
  (nbins,inputImage, outputImage) <- readArgs
  img <- readFromFile inputImage
  let
    gimg = gaussian (5,5) img
    hist = accHistogram nbins $ getValues gimg
    t = tOtsu hist
  saveImage outputImage $ threshold (1,0) t gimg
