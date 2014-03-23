module Main where

import CV.Image

import Thresholding

import ReadArgs
import IOUtils

main = do
  (mode, param, (lower,higher), sourceFile, targetFile) <- readArgs
  img <- readGrayImage sourceFile
  timg <- case mode of
    "basic" -> return $ threshold (lower,higher) param img
    "meandev" -> return $ 
      threshold (lower,higher) (tFromMeanDev param 0.01 img) img
  saveImage targetFile $
    montage (2,1) 4 $
      [ img
      , timg
      ]
