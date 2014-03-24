module Main where

import CV.Image
import CV.Pixelwise
import CV.Operations
import CV.Filters

import Fourier

import ReadArgs

fi = fromIntegral

disc :: Image GrayScale D32
disc  = imageFromFunction (20,20) f
  where
    f (x,y) | r < 5^2   = 1
            | otherwise = 0
            where
              r = ((fi $ x-10)+0.5)^2+((fi $ y-10)+0.5)^2

torus :: Image GrayScale D32
torus  = imageFromFunction (24,24) f
  where
    f (x,y) | r < 9^2 && r > 3^2 = 1
            | otherwise          = 0
            where
              r = ((fi $ x-12)+0.5)^2+((fi $ y-12)+0.5)^2

square :: Image GrayScale D32
square  = imageFromFunction (20,20) f
  where
    f (x,y) | abs ((fi $ x-10)+0.5) < 5 && abs ((fi $ y-10)+0.5) < 5 = 1
            | otherwise                 = 0

diamond :: Image GrayScale D32
diamond  = imageFromFunction (20,20) f
  where
    f (x,y) | (abs $ (fi $ x-10)+0.5)+(abs $ (fi $ y-10)+0.5) < 10 = 1
            | otherwise                 = 0

main = do
  (mode,input,n,resultImage) <- readArgs
  img <- case input of
              "disc" -> return disc
              "gdisc" -> return $ gaussian (3,3) disc
              "torus" -> return torus
              "gtorus" -> return $ gaussian (3,3) torus
              "square" -> return square
              "gsquare" -> return $ gaussian (3,3) square
              "diamond" -> return diamond
              "gdiamond" -> return $ gaussian (3,3) diamond
  let
      (w,h) = getSize img
      (re,im) = dft2D img
      (amp,pha) = dftToPolar2D (re,im)
      inv = idft2D n (re,im)
  rimg <- case mode of
       "complex" -> return $ montage (2,1) 2 [logNormalize re, logNormalize im]
       "polar" -> return $ montage (2,1) 2 [logNormalize amp, unitNormalize pha]
       "inverse" -> return $ inv
       "images" -> return $ montage (w,h) 2 $ fimages (re,im)
       "simages" -> return $ montage (w,h) 2 $ sfimages (re,im)
       "invimages" -> return $ montage (w,h) 2 $ invimages (re,im)
  saveImage resultImage rimg
