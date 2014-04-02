{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import CV.Image
import CV.Operations
import CV.Filters

import DrawingUtils
import Images
import Fourier

import ReadArgs
import Data.Maybe

-- 20pix image of r=5 white disc on black background
disc = discGrayImage 20 5 1 0
sdisc = gaussian (3,3) disc
-- 24pix image of r1=3 and r2=9 white torus on black background
torus = torusGrayImage 24 3 9 1 0
storus = gaussian (3,3) torus
-- 20pix image of r=5 white square on black background
square = squareGrayImage 20 5 1 0
ssquare = gaussian (3,3) square
-- 20pix image of r=5 white diamond on black background
diamond = diamondGrayImage 20 5 1 0
sdiamond = gaussian (3,3) diamond

usage = "bad parameters\n"
     ++ "  usage: l03-fourier2d mode input [n] [scale] result\n"
     ++ "    where\n"
     ++ "    mode = [complex|polar|inverse|dftimages|idftimages|fcompimages]\n"
     ++ "      complex: draws the real and imag part of dft side-by-side\n"
     ++ "      polar:   draws the amp and pha part of dft side-by-side\n"
     ++ "      inverse: draws the result of dft+idft\n"
     ++ "      dftimages: draws the dft using frequency components as pixels\n"
     ++ "      idftimages: draws the idft as the nb of comps is increased\n"
     ++ "      fcompImages: draws the comps in the order used by idftimages\n"
     ++ "   input = [disc|sdisc|torus|storus|square|ssquare|diamond|sdiamond]\n"
     ++ "   n = the number of comps used in idft (sorted by amplitude); 0=all\n"
     ++ "   scale = the scaling factor for complex,polar,inverse; default 8\n"
     ++ "   result = the image where output is drawn\n"

main = do
  (mode :: Maybe String,
   input :: Maybe String,
   numComponents :: Maybe Int,
   scaleFactor :: Maybe Int,
   resultImage :: Maybe String) <- readArgs
  img <- case input of
    Just "disc" -> return disc
    Just "sdisc" -> return sdisc
    Just "torus" -> return torus
    Just "storus" -> return storus
    Just "square" -> return square
    Just "ssquare" -> return ssquare
    Just "diamond" -> return diamond
    Just "sdiamond" -> return sdiamond
    otherwise -> error usage
  -- default value for numComponents is 0
  let
    n = maybe 0 id numComponents
    s = maybe 8 id scaleFactor
    (w,h) = getSize img
    (re,im) = dft2D img
    (amp,pha) = dftToPolar2D (re,im)
    inv = idft2D n (re,im)
  rimg <- case mode of
    Just "complex" -> return $ montage (2,1) 2 $
      [ naiveUpscale s $ logNormalize re
      , naiveUpscale s $ logNormalize im ]
    Just "polar" -> return $ montage (2,1) 2
      [ naiveUpscale s $ logNormalize amp
      , naiveUpscale s $ unitNormalize pha ]
    Just "inverse" -> return $ naiveUpscale s $ inv
    Just "dftimages" -> return $ dftImages (re,im)
    Just "idftimages" -> return $ idftImages (re,im)
    Just "fcompimages" -> return $ fcompImages (re,im)
    otherwise -> error usage
  case resultImage of
    Just fn -> saveImage fn rimg
    otherwise -> error usage
