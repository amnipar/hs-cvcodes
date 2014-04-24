module Main where

import CV.Image

import Math.KMeans
import System.IO.Unsafe
import Control.Monad
import qualified Data.Vector.Unboxed as V

import ReadArgs

import Clustering

main = do
  (mode,k,f,sourceImage,targetImage) <- readArgs
  img <- readFromFile sourceImage
  case mode of
    "value" ->
      saveImage targetImage $ clusterGrayValue k img
    "spatial" ->
      saveImage targetImage $ clusterGraySpatial k f img
    "otherwise" -> error "mode should be value or spatial"
