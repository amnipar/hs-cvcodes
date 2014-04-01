{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import CV.Image
import CV.Pixelwise

import BasicUtils
import DrawingUtils
import Signals

import ReadArgs
import Data.Maybe

-- plot width in pixels
width = 400
-- plot height in pixels
height = 300
-- plot margin
margin = 10
-- plot x scale
xscale = 4*pi
-- plot y scale
yscale = (sum amplitudes) - ymin
-- y value minimum
ymin = min 0 $ (head amplitudes) - (sum $ tail amplitudes)

amplitudes :: [Float]
phases :: [Float]

-- amplitudes a and phases p for b [0..10] frequency components
-- signal is calculated as a[0] + sum of all (a * sin (bx + p*pi))
--          b:   0    1    2    3    4    5    6    7    8    9   10
amplitudes = [10.0, 2.0, 5.0, 0.0, 0.0, 2.0, 0.0, 0.0, 0.0, 0.0, 1.0]
phases =     [ 0.0, 1.0, 0.0, 0.0, 0.0,-0.5, 0.0, 0.0, 0.0, 0.0, 1.0]

usage = "  usage: l01-interpolation mode [samples] [output]\n"
     ++ "    where\n"
     ++ "    mode = [linear|sinc]\n"
     ++ "      linear: use linear interpolation to reconstruct signal\n"
     ++ "      sinc: use sinc interpolation to reconstruct signal\n"
     ++ "    samples = the number of samples taken from signal; default 40\n"
     ++ "    output = the output file name; default result.png\n"

main = do
  ( mode :: Maybe String,
    samples :: Maybe Int,
    output :: Maybe String ) <- readArgs
  let
    outputFile = maybe "result.png" id output
    sampleCount = maybe 40 id samples
    signal = sample (width-2*margin) xscale $ generateSignal amplitudes phases
    points = toPoints (width,height) margin (xscale,yscale) ymin signal
    sampled = sample sampleCount xscale $ generateSignal amplitudes phases
    samplePoints = toPoints (width,height) margin (xscale,yscale) ymin sampled
    y0 = ytop height margin yscale ymin 0
    interpolated = sample (width-2*margin) xscale $
        interpolateSinc sampled xscale
    interpolatedPoints = toPoints (width,height) margin (xscale,yscale) 0 $
        interpolated
  case mode of
    Just "linear" ->
        saveImage outputFile $
          plotLines (1,0,0) 1 samplePoints $
          plotSpikes (0,0,1) 1 2 y0 samplePoints $
          plotLines (0,1,0) 2 points $
          emptyColorImage (width,height) (1,1,1)
    Just "sinc" ->
      saveImage outputFile $
        plotLines (1,0,0) 1 interpolatedPoints $
        plotSpikes (0,0,1) 1 2 y0 samplePoints $
        plotLines (0,1,0) 2 points $
        emptyColorImage (width,height) (1,1,1)
    otherwise -> error $ "bad parameters\n" ++ usage

sinc x | x == 0    = 1
       | otherwise = (sin (pi * x)) / (pi * x)

interpolateSinc :: [(Float,Float)] -> Float -> Float -> Float
interpolateSinc f scale t = sum $ map int $ zip (map iToF [-k..k+1]) f'
  where
    extn = 2*n
    extf = (map ((/(iToF extn)).iToF) [0..extn])
    fsnd = map snd f
    -- pad from beginning and end, and fade linearly to 0
    f' = (zipWith (*) extf $ replicate (extn+1) (head fsnd)) ++
         fsnd ++
         (reverse $ zipWith (*) extf $ replicate (extn+1) (last fsnd))
    n = length f
    n' = length f'
    ninv = 1 / (iToF n')
    k | odd n'     =  n'    `div` 2
      | otherwise  = (n'-1) `div` 2
    offset = (iToF $ extn+1) / (iToF n')
    factor = (iToF n) / (iToF n')
    ts = t/scale*factor
    int (i,fx) = fx * sinc ((ts - i * ninv) / ninv)
