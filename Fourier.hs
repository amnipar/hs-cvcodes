module Fourier 
( oft
, oift
, dftamp
, dftphase
, fimages
, invimages
) where

import CV.Image
import CV.Pixelwise
import CV.Operations
import CV.DFT
import Data.List
import Data.Ord
fi = fromIntegral

sarake :: Image GrayScale D32 -> Int -> [Float]
sarake kuva s = [getPixel (s,y) kuva | y <- [0..h-1]]
  where
        (_,h) = getSize kuva

pikselit :: Image GrayScale D32 -> [(Float,Float,Float)]
pikselit kuva =
  concat $ [[(fi i, fi j, getPixel (i,j) kuva) | j<-[0..h-1]] | i <- [0..w-1]]
  where
    v x y = getPixel (x,y) kuva
    (w,h) = getSize kuva

cpikselit :: Image GrayScale D32 -> Image GrayScale D32
    -> [(Float,Float,Float,Float)]
cpikselit re im =
  concat $ [[(fi i, fi j, getPixel (i,j) re, getPixel (i,j) im)
            | j<-[0..h-1]] | i <- [0..w-1]]
  where
    (w,h) = getSize re

rivi :: Image GrayScale D32 -> Int -> [Float]
rivi kuva r = [getPixel (x,r) kuva | x <- [0..w-1]]
  where
        (w,_) = getSize kuva

kulma :: Float -> Float -> Float -> Float -> (Float,Float,Float) -> (Float,Float)
kulma w h u v (x,y,p) = (p, 2 * pi * (u*x/w + v*y/h))

ckulma :: Float -> Float -> Float -> Float -> (Float,Float,Float,Float)
    -> (Float,Float,Float)
ckulma w h x y (u,v,r,i) = (r,i,2 * pi * (u*x/w + v*y/h))

reaaliosa (v,kulma) = v * cos kulma

imaginaariosa (v,kulma) = - v * sin kulma

realvalue (r,i,a) = (r * cos a, i * sin a)

oft :: Image GrayScale D32 -> (Image GrayScale D32, Image GrayScale D32)
oft kuva = (imageFromFunction (w,h) re, imageFromFunction (w,h) im)
  where
    (w,h) = getSize kuva
    p = pikselit kuva
    re (u,v) = sum $ map reaaliosa $ map (kulma (fi w) (fi h) (fi u) (fi v)) p
    im (u,v) = sum $ map reaaliosa $ map (kulma (fi w) (fi h) (fi u) (fi v)) p

camp :: (Float,Float,Float,Float) -> Float
camp (_,_,re,im) = sqrt $ re*re + im*im

oift :: Int -> (Image GrayScale D32, Image GrayScale D32) -> Image GrayScale D32
oift n (re,im) = imageFromFunction (w,h) inv
  where
    n' | n <= 0 = w*h
       | otherwise = n
    (w,h) = getSize re
    p = reverse $ sortBy (comparing camp) $ cpikselit re im --
    inv (x,y) = (1 / (fi n')) * (sum $ map fst $ map realvalue $
        map (ckulma (fi w) (fi h) (fi x) (fi y)) $ take n' p) --

fimages :: (Image GrayScale D32, Image GrayScale D32) -> [Image GrayScale D32]
fimages (re,im) = map fimg p
  where
    (w,h) = getSize re
    p = cpikselit re im
    fimg cpix = imageFromFunction (w,h) (inv cpix)
    inv cpix (x,y) = snd $ realvalue $ ckulma (fi w) (fi h) (fi x) (fi y) $ cpix

invimages :: (Image GrayScale D32, Image GrayScale D32) -> [Image GrayScale D32]
invimages (re,im) = map (invimg p) [1..n]
  where
    (w,h) = getSize re
    n = w*h
    p = reverse $ sortBy (comparing camp) $ cpikselit re im
    invimg cpix n = imageFromFunction (w,h) (inv cpix n)
    inv p n (x,y) = (1 / (fi n)) * (sum $ map fst $ map realvalue $
        map (ckulma (fi w) (fi h) (fi x) (fi y)) $ take n p)

dftamp :: Image GrayScale D32 -> Image GrayScale D32 -> Image GrayScale D32
dftamp re im = imageFromFunction (w,h) amp
  where
    (w,h) = getSize re
    amp (x,y) = sqrt $ u*u + v*v
      where
        u = getPixel (x,y) re
        v = getPixel (x,y) im

dftphase :: Image GrayScale D32 -> Image GrayScale D32 -> Image GrayScale D32
dftphase re im = imageFromFunction (w,h) phase
  where
    (w,h) = getSize re
    phase (x,y) = atan2 v u
      where
        u = getPixel (x,y) re
        v = getPixel (x,y) im
