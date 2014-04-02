module Main where

import CV.Image

import BasicUtils
import Images
import DrawingUtils
import Signals
import Random
import Fourier

-- plot width in pixels
width = 400
-- plot height in pixels
height = 300
-- plot margin
margin = 10
-- plot x scale
xscale = 4*pi
-- plot y scale; estimate the maximum range of amplitudes
yscale = (sum amplitudes) - ymin
-- minimum value on y axis; zero if estimated amplitude range stays positive
ymin = min 0 $ (head amplitudes) - (sum $ tail amplitudes)
-- standard deviation of the additive gaussian noise
gaussianNoiseSigma = 1.0

amplitudes :: [Float]
phases :: [Float]

-- amplitudes a and phases p for b [0..10] frequency components
-- signal is calculated as a[0] + sum of all (a * sin (bx + p*pi))
--          b:   0    1    2    3    4    5    6    7    8    9   10
amplitudes = [10.0, 2.0, 5.0, 0.0, 0.0, 2.0, 0.0, 0.0, 0.0, 0.0, 1.0]
phases =     [ 0.0, 1.0, 0.0, 0.0, 0.0,-0.5, 0.0, 0.0, 0.0, 0.0, 1.0]

replacex (fx,fy) (x,y) = (x,fy)

snd3 (_,b,_) = b

zeroAmp (u,a,p) = (u,0,p)

ln x | x < 1     = 0
     | otherwise = log x

removeHighest n s = s1 ++ (map zeroAmp s2) ++ s3
  where
    l = length s
    c | even l = l `div` 2
      | otherwise = l `div` 2 + 1
    s1 = take (c-n) s
    s2 = take (2*n) $ drop (c-n) s
    s3 = drop (c+n) s

main = do
  let
    signal = sample (width-2*margin) xscale $ generateSignal amplitudes phases
    corrupted = corruptSignalWithGaussian gaussianNoiseSigma signal
    clean = signalToPixel (width,height) margin (xscale,yscale) ymin signal
    points = signalToPixel (width,height) margin (xscale,yscale) ymin corrupted
    ipoints = signalToPixel (width,height) margin (xscale,yscale) ymin isignal
    fsignal = dft1D corrupted
    psignal = removeHighest 160 $ dftToPolar1D fsignal
    ppoints = signalToPixel (width,height) margin (xscale,yscale) ymin $
      zip (map fst signal) (map ln $ map snd3 psignal)
    isignal = zip (map fst corrupted) $ (idft1D 0 $ polarToDft1D psignal)
    y0 = ytop height margin yscale ymin 0
  saveImage "fourier-signal.png" $
      plotLines blue 1 ipoints $
      plotSpikes red 1 0 y0 ppoints $
      plotLines green 2 clean $
      --plotLines (1,0,0) 1 noise $
      emptyColorImage (width,height) (1,1,1)
