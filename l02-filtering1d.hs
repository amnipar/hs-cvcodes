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

import BasicUtils
import DrawingUtils
import Signals
import Random
import Filters

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
mask :: [Float]
mask = gaussianMask1D(maskSize)
--mask = averageMask1D(maskSize)

-- size of the convolution mask
maskSize :: Int
maskSize = 7

-- index of the mask center
maskCenter :: Int
maskCenter = 3

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
    corrupted = corruptSignalWithGaussian gaussianNoiseSigma signal
    filtered = convolve1D mask maskCenter corrupted
    cleanPoints = toPoints (width,height) margin (xscale,yscale) ymin signal
    corruptedPoints = toPoints (width,height) margin (xscale,yscale) ymin corrupted
    filteredPoints = toPoints (width,height) margin (xscale,yscale) ymin filtered
  saveImage "filtered-signal.png" $
      plotLines blue 1 filteredPoints $
      plotLines red 1 corruptedPoints $
      plotLines green 2 cleanPoints $
      emptyColorImage (width,height) white
