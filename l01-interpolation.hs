module Main where

import CV.Image
import CV.Pixelwise

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
yscale = (sum amplitudes) - ymin
-- y value minimum
ymin = min 0 $ (head amplitudes) - (sum $ tail amplitudes)
-- number of samples
sampleCount = 40

amplitudes :: [Float]
phases :: [Float]

-- amplitudes a and phases p for b [0..10] frequency components
-- signal is calculated as a[0] + sum of all (a * sin (bx + p*pi))
--          b:   0    1    2    3    4    5    6    7    8    9   10
amplitudes = [10.0, 2.0, 5.0, 0.0, 0.0, 2.0, 0.0, 0.0, 0.0, 0.0, 1.0]
phases =     [ 0.0, 1.0, 0.0, 0.0, 0.0,-0.5, 0.0, 0.0, 0.0, 0.0, 1.0]

main = do
  let
    signal = sample (width-2*margin) xscale $ generateSignal amplitudes phases
    points = toPoints (width,height) margin (xscale,yscale) ymin signal
    sampled = sample sampleCount xscale $ generateSignal amplitudes phases
    samplePoints = toPoints (width,height) margin (xscale,yscale) ymin sampled
    y0 = ytop height margin yscale ymin 0
  saveImage "interpolation.png" $
      plotLines (1,0,0) 1 samplePoints $
      plotSpikes (0,0,1) 1 2 y0 samplePoints $
      plotLines (0,1,0) 2 points $
      emptyColorImage (width,height) (1,1,1)

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
