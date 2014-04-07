{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import CV.Image
import qualified CV.ImageMath as IM
import CV.ImageMathOp
import CV.Filters
import CV.Operations
import CV.Pixelwise
import CV.ColourUtils

import Filters
import Gabor
import Fourier

import ReadArgs
import Data.Maybe

gSize = 7
gCenter = getMaskCenter2D gSize
gMasks = [g1,g2,g3,g4,g5,g6,g7,g8]

-- | Calculate the covariance of image regions between an image and a list of
--   other images. The size of considered image regions can be given.
covariance :: (Int,Int) -> Image GrayScale Float -> [Image GrayScale Float]
    -> [Image GrayScale Float]
covariance s x ys = map (cov x ex) ys
  where
    ex = gaussian s x
    cov x ex y = exy #- (ex #* ey)
      where
        xy = x #* y
        exy = gaussian s xy
        ey = gaussian s y

-- | Calculates the Pearson correlation of image regions between an image and a
--   list of other images. The size of considered image regions can be given.
correlation :: (Int,Int) -> Image GrayScale Float -> [Image GrayScale Float]
    -> [Image GrayScale Float]
correlation s x ys = map (corr x ex sx) ys
  where
    ex = gaussian s x
    sx = (gaussian s (x #* x)) #- (ex #* ex)
    corr x ex sx y = IM.div (exy #- (ex #* ey)) (IM.maxS 0.001 $ IM.sqrt $ sx #* sy)
      where
        xy = x #* y
        exy = gaussian s xy
        ey = gaussian s y
        sy = (gaussian s (y #* y)) #- (ey #* ey)

-- | Gets the normalized amplitude of Gabor filter response from an image.
getGaborAmp size center image mask =
  stretchHistogram gamp
  where
    (gre,gim) = filterGabor mask size center image
    (gamp,_) = dftToPolar2D (gre,gim)

usage :: String
usage = "bad parameters\n"
     ++ "  usage: l04-regionstat mode size input output\n"
     ++ "    where\n"
     ++ "    mode = [stat|scov|scorr|gcov|gcorr]\n"
     ++ "      stat:  calculate mean, variance and stddev of image regions\n"
     ++ "      scov:  calculate covariance of input and its 1-pixel shifts\n"
     ++ "      scorr: calculate correlation of input and its 1-pixel shifts\n"
     ++ "      gcov:  calculate covariance of gabor filters of the input\n"
     ++ "      gcorr: calculate correlation of gabor filters of the input\n"
     ++ "    size = the size of image region to consider, default 5\n"
     ++ "    input = the input image to analyze\n"
     ++ "    output = the output image where the result is drawn\n"

main = do
  ( mode :: Maybe String,
    regionSize :: Maybe Int,
    inputImage :: Maybe String,
    outputImage :: Maybe String ) <- readArgs
  let
    input = maybe "nut.png" id inputImage
  img00 <- readFromFile(input)
  let
    rs = maybe 5 id regionSize
    output = maybe "result.png" id outputImage
    gabors = map (getGaborAmp gSize gCenter img00) gMasks
    size = (rs,rs)
    img10 = shift (1,0) img00
    img01 = shift (0,1) img00
    img11 = shift (1,1) img00
    x = img00
    x2 = x #* x
    mu1 = gaussian size x
    sigma2 = (gaussian size x2) #- (mu1 #* mu1)
    sigma1 = IM.sqrt sigma2
  case mode of
    Just "stat" ->
      saveImage output $ montage (2,2) 2 $ [x, mu1, sigma2, sigma1]
    Just "scov" ->
      saveImage output $ montage (2,2) 2 $ map unitNormalize $
        [x] ++ covariance size img00 [img10,img01,img11]
    Just "scorr" ->
      saveImage output $ montage (2,2) 2 $ map unitNormalize $
        [x] ++ correlation size img00 [img10,img01,img11]
    Just "gcov" ->
      saveImage output $ montage (4,4) 2 $ map unitNormalize $
        gabors ++ covariance size (head gabors) gabors
    Just "gcorr" ->
      saveImage output $ montage (4,4) 2 $ map unitNormalize $
        gabors ++ correlation size (head gabors) gabors
    otherwise -> error usage

-- | Shifts an image by the given amount of pixels.
shift :: (Int,Int) -> Image GrayScale Float -> Image GrayScale Float
shift (sx,sy) image = imageFromFunction (w,h) f
  where
    (w,h) = getSize image
    f (x,y) | x < sx || y < sy = getPixel (x,y) image
            | otherwise        = getPixel (x-sx,y-sy) image
