module Main where

import CV.Image
import CV.Pixelwise
import qualified CV.ImageMath as IM
import CV.ImageOp
import CV.DFT as DFT
import CV.Drawing
import CV.Filters

import Data.Function
import Data.List
import Control.Monad
import Control.Applicative
import System.Random
import System.IO.Unsafe
import Debug.Trace
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
yscale = (sum amplitudes) - ymin -- 2 * (sum $ tail amplitudes)
ymin = min 0 $ (head amplitudes) - (sum $ tail amplitudes) -- -(sum $ tail amplitudes)
-- standard deviation of the additive gaussian noise
gaussianNoiseSigma = 1.0

-- mask used in convolving the signal
-- either averageMask or gaussianMask can be selected, n tells the mask length
mask = averageMask 7

averageMask :: Int -> [Float]
averageMask n = replicate n (1 / (fi n))

gaussianMask :: Int -> [Float]
gaussianMask n = createMask g n

amplitudes :: [Float]
phases :: [Float]

-- amplitudes a and phases p for b [0..10] frequency components
-- signal is calculated as a[0] + sum of all (a * sin (bx + p*pi))
--          b:   0    1    2    3    4    5    6    7    8    9   10
amplitudes = [10.0, 2.0, 5.0, 0.0, 0.0, 2.0, 0.0, 0.0, 0.0, 0.0, 1.0]
phases =     [ 0.0, 1.0, 0.0, 0.0, 0.0,-0.5, 0.0, 0.0, 0.0, 0.0, 1.0]

fi = fromIntegral

sample :: Int -> Float -> (Float -> Float) -> [(Float,Float)]
sample n scale f = map (s f) domain
  where
        s f x = (x, f x)
        domain = [scale * (((fi x) - (fi n / 2)) / (fi n)) | x <- [0..n]]

convolve :: [Float] -> [(Float,Float)] -> [(Float,Float)]
convolve g f = zip xs $ take (length xs) gy
  where
    xs = map fst f
    ys = map snd f
    -- need to pad the signal from both ends to make the kernel fit
    ys' = (replicate c (head ys)) ++ ys ++ (replicate c (last ys))
    gy = map (sum . zipWith ( * ) (reverse g)) (init $ tails ys')
    l = length g
    c = l `div` 2

generateSignal :: [Float] -> [Float] -> Float -> Float
generateSignal (m:ms) (p:ps) x =
  m + (sum $ map (s x) $ zip3 ms ps [1..10])
  where
    s :: Float -> (Float,Float,Float) -> Float
    s x (a,p,i) = a * sin (i*x+p*pi)

g :: Float -> Int -> Float
g s x = (1 / (sqrt $ 2 * pi * s**2)) * exp (-((fi x)**2)/(2 * s**2))

kernel :: (Int -> Float) -> Int -> [Float]
kernel f r = [f x | x <- [-r..r]]

createMask :: (Float -> Int -> Float) -> Int -> [Float]
createMask f l = kernel (f s) r
  where
    r | even l = (l `div` 2) - 1
      | otherwise = (l `div` 2)
    s = (fi l) / 6

yToFloat (x,y) = (x,realToFrac y)
yToDouble (x,y) = (x,realToFrac y)

main = do
  rng <- newRNG mt19937
  setSeed rng 12349871235125
  gs <- generateNGaussians rng gaussianNoiseSigma (width - 2*margin)
  let
    signal = sample (width-2*margin) xscale $ generateSignal amplitudes phases
    corrupted = map yToFloat $ corruptGaussian (map yToDouble signal) gs
    clean = toPoints (width,height) margin (xscale,yscale) ymin signal
    --noise = zip [margin,width-margin-1] $
    --    map (floor.(\y -> y*yscale + (fi $ height-y0-10))) gs
    points = toPoints (width,height) margin (xscale,yscale) ymin corrupted
    filtered = convolve mask corrupted
    filteredPoints = toPoints (width,height) margin (xscale,yscale) ymin filtered
  saveImage "averaged-signal.png" $
      plotLines (1,0,0) 1 filteredPoints $
      plotLines (0,0,1) 1 points $
      plotLines (0,1,0) 2 clean $
      --plotLines (1,0,0) 1 noise $
      emptyColorImage (width,height) (1,1,1)
