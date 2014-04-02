module Main where

import CV.Image
import CV.Pixelwise

import Images
import DrawingUtils
import Random

-- plot width in pixels
width = 400
-- plot height in pixels
height = 300
-- plot margin
margin = 10
-- plot x scale
xscale = 4*pi
-- plot y scale
yscale = 2 * (sum $ tail amplitudes)
-- y value minimum
ymin = -(sum $ tail amplitudes)

amplitudes :: [Float]
phases :: [Float]

-- amplitudes a and phases p for b [0..10] frequency components
-- signal is calculated as a[0] + sum of all (a * sin (bx + p*pi))
--          b:   0    1    2    3    4    5    6    7    8    9   10
amplitudes = [ 0.0, 2.0, 3.0, 0.0, 0.0, 2.0, 0.0, 0.0, 0.0, 0.0, 1.0]
phases =     [ 0.0, 1.0, 0.0, 0.0, 0.0,-0.5, 0.0, 0.0, 0.0, 0.0, 1.0]

main = do
  let
    signal = sample (width-2*margin) xscale $ generateSignal amplitudes phases
    points = signalToPixel (width,height) margin (xscale,yscale) ymin signal
    c1  = getComponentPoints 1
    c2  = getComponentPoints 2
    c3  = getComponentPoints 3
    c4  = getComponentPoints 4
    c5  = getComponentPoints 5
    c6  = getComponentPoints 6
    c7  = getComponentPoints 7
    c8  = getComponentPoints 8
    c9  = getComponentPoints 9
    c10 = getComponentPoints 10
  saveImage "signal.png" $
      plotLines (0,0,1) 1 c1 $
      plotLines (0,0,1) 1 c2 $
      plotLines (0,0,1) 1 c3 $
      plotLines (0,0,1) 1 c4 $
      plotLines (0,0,1) 1 c5 $
      plotLines (0,0,1) 1 c6 $
      plotLines (0,0,1) 1 c7 $
      plotLines (0,0,1) 1 c8 $
      plotLines (0,0,1) 1 c9 $
      plotLines (0,0,1) 1 c10 $
      plotLines (0,1,0) 2 points $
      emptyColorImage (width,height) (1,1,1)

generateFrequencyComponent :: [Float] -> [Float] -> Int -> Float -> Float
generateFrequencyComponent ms ps i x
  | length ms < (i+1) || length ps < length ms || i < 0 = 0
  | i == 0 = head ms
  | otherwise = (ms !! i) * sin ((fi i) * x + (ps !! i)*pi)

generateSignal :: [Float] -> [Float] -> Float -> Float
generateSignal (m:ms) (p:ps) x =
  m + (sum $ map (s x) $ zip3 ms ps [1..10])
  where
    s :: Float -> (Float,Float,Float) -> Float
    s x (a,p,i) = a * sin (i*x+p*pi)

fi = fromIntegral

sample :: Int -> Float -> (Float -> Float) -> [(Float,Float)]
sample n scale f = map (s f) domain
  where
        s f x = (x, f x)
        domain = [scale * (((fi x) - (fi n / 2)) / (fi n)) | x <- [0..n]]

getComponentPoints c = 
  signalToPixel (width,height) margin (xscale,yscale) ymin $
      sample (width-2*margin) xscale $
      generateFrequencyComponent amplitudes phases c
