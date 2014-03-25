module Fourier
( dft1D
, idft1D
, dftToPolar1D
, polarToDft1D
, dft2D
, idft2D
, dftToPolar2D
, polarToDft2D
, dftImages
, fcompImages
, idftImages
) where

import CV.Image
import CV.Pixelwise
import Data.List
import Data.Ord

import BasicUtils

-- | Transforms an image to a list of pixels for mapping
pixels :: Image GrayScale D32 -> [(Float,Float,Float)]
pixels image =
  concat $ [[(iToF i, iToF j, getPixel (i,j) image)
            | j<-[0..h-1]] | i <- [0..w-1]]
  where
    (w,h) = getSize image

-- | Transforms a dft pair into a list of complex pixels for mapping
cpixels :: Image GrayScale D32 -> Image GrayScale D32
    -> [(Float,Float,Float,Float)]
cpixels re im =
  concat $ [[(iToF i, iToF j, getPixel (i,j) re, getPixel (i,j) im)
            | j<-[0..h-1]] | i <- [0..w-1]]
  where
    (w,h) = getSize re

-- | Calculates the forward frequency component angle based on u,x,n
toAngle1D :: Float -> Float -> (Float,Float) -> (Float,Float)
toAngle1D n u (x,f) = (f, 2 * pi * (u*x/n))

-- | Calculates the backward frequency component angle based on u,x,n
toCAngle1D :: Float -> Float -> (Float,Float,Float) -> (Float,Float,Float)
toCAngle1D n x (u,r,i) = (r, i, 2 * pi * (u*x/n))

-- | Calculates the forward frequency component angle based on u,v,x,y,w,h
toAngle2D :: Float -> Float -> Float -> Float -> (Float,Float,Float)
    -> (Float,Float)
toAngle2D w h u v (x,y,f) = (f, 2 * pi * (u*x/w + v*y/h))

-- | Calculates the backward frequency component angle based on u,v,x,y,w,h
toCAngle2D :: Float -> Float -> Float -> Float -> (Float,Float,Float,Float)
    -> (Float,Float,Float)
toCAngle2D w h x y (u,v,r,i) = (r, i, 2 * pi * (u*x/w + v*y/h))

-- | Calculates the real part of fourier coefficient based on value and angle
toReal :: (Float,Float) -> Float
toReal (f,a) = f * cos a

-- | Calculates the imag part of fourier coefficient based on value and angle
toImag :: (Float,Float) -> Float
toImag (f,a) = -f * sin a

-- | Calculates the (real) inverse fourier based on real, imag and angle
cToR :: (Float,Float,Float) -> Float
cToR (r,i,a) = r * cos a - i * sin a

-- | Calculates a 1D fourier transform of a list of x,fx pairs
dft1D :: [(Float,Float)] -> [(Float,Float,Float)]
dft1D signal = map toFourier (map iToF [0..n-1])
  where
    -- replace signal x values with new values between 0..n-1
    values = zip (map iToF [0..n-1]) (map snd signal)
    -- get the frequency component angles corresponding to each value
    angles u = map (toAngle1D (iToF n) u) values
    n = length signal
    toFourier :: Float -> (Float,Float,Float)
    toFourier u = (u, re, im)
      where
        -- get the real part of the result based on angles
        re = sum $ map toReal $ angles u
        -- get the imaginary part of the result based on angles
        im = sum $ map toImag $ angles u

-- | Calculates a 1D inverse fourier of a list of u,re,im triples.
--   If n is not 0, uses only abs(n) coefficients in inverse transform.
--   If n<0, first sorts coefficients by magnitude (takes n strongest ones).
--   If n>0, just takes n first coefficients (largest frequencies).
idft1D :: Int -> [(Float,Float,Float)] -> [Float]
idft1D n f = map fromFourier [0..l-1]
  where
    l = length f
    n' | n == 0    = l
       | otherwise = abs n
    f' | n == 0    = f
       | n < 0     = take n' $ reverse $ sortBy (comparing amp) f
       | otherwise = take n' f
    amp (_,re,im) = sqrt $ re^2 + im^2
    -- gets the inverse fourier by mapping to angles and values, then summing
    fromFourier x = (1 / (iToF l)) * (sum $
        map (cToR.(toCAngle1D (iToF l) (iToF x))) f')

-- | Converts a complex signal to polar (amplitude,phase) coordinates
dftToPolar1D :: [(Float,Float,Float)] -> [(Float,Float,Float)]
dftToPolar1D s = map toPolar s
  where
    toPolar (u, re, im) = (u, (sqrt $ re^2 + im^2), (atan2 im re))

-- | Converts a polar representation to a complex signal
polarToDft1D :: [(Float,Float,Float)] -> [(Float,Float,Float)]
polarToDft1D s = map toDft s
  where
    toDft (u, r, a) = (u, r * cos a, r * sin a)

-- | Calculates a 2D fourier transform from an image.
--   The result a pair of images (re,im) representing the real and imag part
--   of the fourier coefficients.
dft2D :: Image GrayScale D32 -> (Image GrayScale D32, Image GrayScale D32)
dft2D image = (imageFromFunction (w,h) re, imageFromFunction (w,h) im)
  where
    (w,h) = getSize image
    values = pixels image
    -- get the frequency component angles corresponding to each value
    angles u v = map (toAngle2D (iToF w) (iToF h) (iToF u) (iToF v)) values
    -- get the real part of the coefficient based on angles
    re (u,v) = sum $ map toReal $ angles u v
    -- get the imag part of the coefficient based on angles
    im (u,v) = sum $ map toImag $ angles u v

-- | Calculates a 2D inverse fourier of a (re,im) pair of images.
--   If n is not 0, sorts the coefficients by amplitude and uses only the n
--   strongest coefficients in the inverse transform.
idft2D :: Int -> (Image GrayScale D32, Image GrayScale D32) -> Image GrayScale D32
idft2D n (re,im) = imageFromFunction (w,h) fromFourier
  where
    (w,h) = getSize re
    p = cpixels re im
    n' | n <= 0    = w*h
       | otherwise = n
    p' | n <= 0    = p
       | otherwise = take n' $ reverse $ sortBy (comparing amp) $ p
    amp (_,_,re,im) = sqrt $ re^2 + im^2
    fromFourier (x,y) = (1 / (iToF n')) * (sum $
        map (cToR.(toCAngle2D (iToF w) (iToF h) (iToF x) (iToF y))) p')

-- | Converts a (re,im) complex image pair to polar (amplitude,phase) pair
dftToPolar2D :: (Image GrayScale D32, Image GrayScale D32) ->
    (Image GrayScale D32, Image GrayScale D32)
dftToPolar2D (re,im) = (dftamp re im, dftpha re im)

-- | Converts a polar (amplitude,phase) image pair to complex (re,im) pair
polarToDft2D :: (Image GrayScale D32, Image GrayScale D32) ->
  (Image GrayScale D32, Image GrayScale D32)
polarToDft2D (amp,pha) = (dftre amp pha, dftim amp pha)

-- | Creates an array of images from a (re,im) complex image pair. Each image
--   represents one frequency component corresponding to one fourier
--   coefficient. The frequency components will be in the same order as the
--   pixels in the complex image. Warning: suitable only for small images.
dftImages :: (Image GrayScale D32, Image GrayScale D32) -> Image GrayScale D32
dftImages (re,im) = montage (w,h) 2 $ map fimg p
  where
    (w,h) = getSize re
    p = cpixels re im
    fimg cpix = imageFromFunction (w,h) (inv cpix)
    inv cpix (x,y) = cToR.(toCAngle2D (iToF w) (iToF h) (iToF x) (iToF y))$ cpix

-- | Creates an array of images from a (re,im) complex image pair. Each image
--   represents one frequency component corresponding to one fourier
--   coefficient. The frequency components will be sorted by amplitude, so the
--   strongest components will be shown first.
fcompImages :: (Image GrayScale D32, Image GrayScale D32) -> Image GrayScale D32
fcompImages (re,im) = montage (w,h) 2 $ map fimg p
  where
    (w,h) = getSize re
    p = reverse $ sortBy (comparing amp) $ cpixels re im
    amp (_,_,re,im) = sqrt $ re^2 + im^2
    fimg cpix = imageFromFunction (w,h) (inv cpix)
    inv cpix (x,y) = cToR.(toCAngle2D (iToF w) (iToF h) (iToF x) (iToF y))$ cpix

-- | Creates a composite array of images from a (re,im) complex image pair. Each
--   image represents the inverse transform of a subset of the fourier
--   coefficients. The frequency components are first sorted by amplitude, then
--   the first image is generated based only on the strongest component, and
--   then one image is added at a time. In the final image, all coefficients are
--   used.
idftImages :: (Image GrayScale D32, Image GrayScale D32) -> Image GrayScale D32
idftImages (re,im) = montage (w,h) 2 $ map (invimg p) [1..n]
  where
    (w,h) = getSize re
    n = w*h
    p = reverse $ sortBy (comparing amp) $ cpixels re im
    amp (_,_,re,im) = sqrt $ re^2 + im^2
    invimg cpix n = imageFromFunction (w,h) (inv cpix n)
    inv p n (x,y) = (1 / (iToF n)) * (sum $
        map (cToR.(toCAngle2D (iToF w) (iToF h) (iToF x) (iToF y))) $ take n p)

-- | Calculates the amplitude image from a (re,im) pair.
dftamp :: Image GrayScale D32 -> Image GrayScale D32 -> Image GrayScale D32
dftamp re im = imageFromFunction (w,h) amp
  where
    (w,h) = getSize re
    amp (x,y) = sqrt $ u*u + v*v
      where
        u = getPixel (x,y) re
        v = getPixel (x,y) im

-- | Calculates the phase image from a (re,im) pair.
dftpha :: Image GrayScale D32 -> Image GrayScale D32 -> Image GrayScale D32
dftpha re im = imageFromFunction (w,h) pha
  where
    (w,h) = getSize re
    pha (x,y) = atan2 v u
      where
        u = getPixel (x,y) re
        v = getPixel (x,y) im

-- | Calculates the real part of a complex image from a (amplitude,phase) pair.
dftre :: Image GrayScale D32 -> Image GrayScale D32 -> Image GrayScale D32
dftre amp pha = imageFromFunction (w,h) re
  where
    (w,h) = getSize amp
    re (x,y) = r * cos a
      where
        r = getPixel (x,y) amp
        a = getPixel (x,y) pha

-- | Calculates the imag part of a complex image from a (amplitude,phase) pair.
dftim :: Image GrayScale D32 -> Image GrayScale D32 -> Image GrayScale D32
dftim amp pha = imageFromFunction (w,h) im
  where
    (w,h) = getSize amp
    im (x,y) = r * sin a
      where
        r = getPixel (x,y) amp
        a = getPixel (x,y) pha
