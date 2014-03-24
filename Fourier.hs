module Fourier
( dft1D
, idft1D
, dftToPolar1D
, dft2D
, idft2D
, dftToPolar2D
, fimages
, sfimages
, invimages
) where

import CV.Image
import CV.Pixelwise
import CV.Operations
import CV.DFT
import Data.List
import Data.Ord

fi = fromIntegral

pixels :: Image GrayScale D32 -> [(Float,Float,Float)]
pixels image =
  concat $ [[(fi i, fi j, getPixel (i,j) image) | j<-[0..h-1]] | i <- [0..w-1]]
  where
    (w,h) = getSize image

cpixels :: Image GrayScale D32 -> Image GrayScale D32
    -> [(Float,Float,Float,Float)]
cpixels re im =
  concat $ [[(fi i, fi j, getPixel (i,j) re, getPixel (i,j) im)
            | j<-[0..h-1]] | i <- [0..w-1]]
  where
    (w,h) = getSize re

angle1D :: Float -> Float -> (Float,Float) -> (Float,Float)
angle1D n u (x,f) = (f, 2 * pi * (u*x/n))

cangle1D :: Float -> Float -> (Float,Float,Float) -> (Float,Float,Float)
cangle1D n x (u,r,i) = (r, i, 2 * pi * (u*x/n))

angle2D :: Float -> Float -> Float -> Float -> (Float,Float,Float)
    -> (Float,Float)
angle2D w h u v (x,y,f) = (f, 2 * pi * (u*x/w + v*y/h))

cangle2D :: Float -> Float -> Float -> Float -> (Float,Float,Float,Float)
    -> (Float,Float,Float)
cangle2D w h x y (u,v,r,i) = (r, i, 2 * pi * (u*x/w + v*y/h))

toReal (f,a) = f * cos a

toImag (f,a) = -f * sin a

cToR (r,i,a) = r * cos a - i * sin a

dft1D :: [(Float,Float)] -> [(Float,Float,Float)]
dft1D signal = map f $ [0..n-1]
  where
    signal' = zip (map fromIntegral [0..n-1]) (map snd signal)
    n = length signal
    f u = ((fi u), re, im)
      where
        re = sum $ map toReal $ map (angle1D (fi n) (fi u)) signal'
        im = sum $ map toImag $ map (angle1D (fi n) (fi u)) signal'

dftToPolar1D :: [(Float,Float,Float)] -> [(Float,Float,Float)]
dftToPolar1D s = map toPolar s
  where
    toPolar (u, re, im) = (u, (sqrt $ re^2 + im^2), (atan2 im re))

idft1D :: Int -> [(Float,Float,Float)] -> [Float]
idft1D n f = map inv [0..l-1]
  where
    l = length f
    n' | n == 0    = l
       | otherwise = abs n
    f' | n == 0    = f
       | n < 0     = take n' $ reverse $ sortBy (comparing amp) f
       | otherwise = take n' f
    amp (_,re,im) = sqrt $ re*re + im*im
    inv x = (1 / (fi l)) * (sum $ map cToR $ map (cangle1D (fi l) (fi x)) f')

dft2D :: Image GrayScale D32 -> (Image GrayScale D32, Image GrayScale D32)
dft2D image = (imageFromFunction (w,h) re, imageFromFunction (w,h) im)
  where
    (w,h) = getSize image
    p = pixels image
    re (u,v) = sum $ map toReal $ map (angle2D (fi w) (fi h) (fi u) (fi v)) p
    im (u,v) = sum $ map toImag $ map (angle2D (fi w) (fi h) (fi u) (fi v)) p

idft2D :: Int -> (Image GrayScale D32, Image GrayScale D32) -> Image GrayScale D32
idft2D n (re,im) = imageFromFunction (w,h) inv
  where
    (w,h) = getSize re
    p = cpixels re im
    n' | n <= 0    = w*h
       | otherwise = n
    p' | n <= 0    = p
       | otherwise = take n' $ reverse $ sortBy (comparing amp) $ p
    amp (_,_,re,im) = sqrt $ re*re + im*im
    inv (x,y) = (1 / (fi n')) * (sum $ map cToR $
        map (cangle2D (fi w) (fi h) (fi x) (fi y)) p')

dftToPolar2D :: (Image GrayScale D32, Image GrayScale D32) ->
    (Image GrayScale D32, Image GrayScale D32)
dftToPolar2D (re,im) = (dftamp re im, dftpha re im)

fimages :: (Image GrayScale D32, Image GrayScale D32) -> [Image GrayScale D32]
fimages (re,im) = map fimg p
  where
    (w,h) = getSize re
    p = cpixels re im
    fimg cpix = imageFromFunction (w,h) (inv cpix)
    inv cpix (x,y) = cToR $ cangle2D (fi w) (fi h) (fi x) (fi y) $ cpix

sfimages :: (Image GrayScale D32, Image GrayScale D32) -> [Image GrayScale D32]
sfimages (re,im) = map fimg p
  where
    (w,h) = getSize re
    p = reverse $ sortBy (comparing amp) $ cpixels re im
    amp (_,_,re,im) = sqrt $ re*re + im*im
    fimg cpix = imageFromFunction (w,h) (inv cpix)
    inv cpix (x,y) = cToR $ cangle2D (fi w) (fi h) (fi x) (fi y) $ cpix

invimages :: (Image GrayScale D32, Image GrayScale D32) -> [Image GrayScale D32]
invimages (re,im) = map (invimg p) [1..n]
  where
    (w,h) = getSize re
    n = w*h
    p = reverse $ sortBy (comparing amp) $ cpixels re im
    amp (_,_,re,im) = sqrt $ re*re + im*im
    invimg cpix n = imageFromFunction (w,h) (inv cpix n)
    inv p n (x,y) = (1 / (fi n)) * (sum $ map cToR $
        map (cangle2D (fi w) (fi h) (fi x) (fi y)) $ take n p)

dftamp :: Image GrayScale D32 -> Image GrayScale D32 -> Image GrayScale D32
dftamp re im = imageFromFunction (w,h) amp
  where
    (w,h) = getSize re
    amp (x,y) = sqrt $ u*u + v*v
      where
        u = getPixel (x,y) re
        v = getPixel (x,y) im

dftpha :: Image GrayScale D32 -> Image GrayScale D32 -> Image GrayScale D32
dftpha re im = imageFromFunction (w,h) pha
  where
    (w,h) = getSize re
    pha (x,y) = atan2 v u
      where
        u = getPixel (x,y) re
        v = getPixel (x,y) im
