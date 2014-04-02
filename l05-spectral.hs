{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import CV.Image
import CV.Transforms
import CV.ImageMath as IM
import CV.Operations
import CV.ImageOp
import CV.Drawing
import Utils.Rectangle

import Data.List
import Data.Maybe
import Control.Applicative

import Text.Printf
import ReadArgs

import IOUtils
import Images
import DrawingUtils

-- this is needed to remove the bandwidth label text from images
removeBandLabel img =
  img <# rectOp 0.5 (-1) (mkRectangle (100,55) (45,11))

usage = "  usage: l05-spectral mode [band1: 1..64] [band2: 1..64] result\n"
     ++ "    where\n"
     ++ "    mode = [bands|ratio]\n"
     ++ "      bands: draws a montage of all 64 bands in folder spectral\n"
     ++ "      ratio: shows the ratio [band1] / [band2] as an image\n"
     ++ "    band1: the index of first band for operations on bands\n"
     ++ "    band2: the index of the second for operations on two bands\n"

main = do
  ( mode :: Maybe String,
    band1 :: Maybe Int,
    band2 :: Maybe Int,
    outputFile :: Maybe String) <- readArgs
  let
    b1 = maybe 0 id band1
    b2 = maybe 0 id band2
    output = maybe "result.png" id outputFile
  case mode of
    -- show a montage of all 64 bands in an image
    Just "bands" -> do
      files <- getFilesInPath "./plant/" ".png"
      images <- mapM readGrayImage files
      saveImage output $ montage (8,8) 2 $ map (resizeImage (80,60)) images
    -- show the ratio of two bands in an image
    Just "ratio" -> do
      if b1 > 0 && b1 < 65 && b2 > 0 && b2 < 65
          then do
            img1 <- readGrayImage $ printf "./plant/plant-%02d.png" b1
            img2 <- readGrayImage $ printf "./plant/plant-%02d.png" b2
            saveImage output $ unitNormalize $ IM.div
                (removeBandLabel img1) (removeBandLabel img2)
          else error $ "band numbers must be in range [1..64]\n" ++ usage
    otherwise -> error $ "bad parameters\n" ++ usage
