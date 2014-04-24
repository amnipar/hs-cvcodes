module Main where

import CV.Image

import Thresholding
import Graph

import ReadArgs

main = do
  (mode,sourceImage,targetImage) <- readArgs
  img <- readFromFile sourceImage
  case mode of
    "connected" ->
      saveImage targetImage $ findConnectedComponents $ threshold (1,0) 0.5 img
    "spanning" ->
      saveImage targetImage $ findSpanningForest 0.05 img
