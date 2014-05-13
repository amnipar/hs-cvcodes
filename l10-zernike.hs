module Main where

import CV.Image
import CV.Operations

import BasicUtils
import Images
import Histogram
import Thresholding

import ReadArgs
import System.IO.Unsafe

pixelToRadial :: (Int,Int) -> Int -> ((Int,Int),Float)
    -> ((Int,Int),(Float,Float),Float)
pixelToRadial (cx,cy) maxr ((x,y),v) = ((x,y),(r,a'),v)
  where
    x' = iToF $ x-cx
    y' = iToF $ y-cy
    r = (sqrt $ x'**2 + y'**2) / (iToF maxr)
    a = atan2 y' x'
    a' | a < 0     = a + 2*pi
       | otherwise = a

zp n m r
  | n == 0 && m == 0 = 1
  | n == 1 && m == 0 = -2 + 3 * r
  | n == 1 && m == 1 = r
  | n == 2 && m == 0 = 3 + 10 * r**2 - 12 * r
  | n == 2 && m == 1 = 5 * r**2 + 4 * r
  | n == 2 && m == 2 = r**2
  | n == 3 && m == 0 = -4 + 35 * r**3 - 60 * r**2 + 30 * r
  | n == 3 && m == 1 = 21 * r**3 - 30 * r**2 + 10 * r
  | n == 3 && m == 2 = 7 * r**3 - 6 * r**2
  | n == 3 && m == 3 = r**3
  | n == 4 && m == 0 = 5 + 126 * r**4 - 280 * r**3 + 210 * r**2 - 60 * r
  | n == 4 && m == 1 = 84 * r**4 - 168 * r**3 + 105 * r**2 - 20 * r
  | n == 4 && m == 2 = 36 * r**4 - 56 * r**3 + 21 * r**2
  | n == 4 && m == 3 = 9 * r**4 - 8 * r**3
  | n == 4 && m == 4 = r**4
  | n == 5 && m == 0 = -6 + 462 * r**5 - 1260 * r**4 + 1260 * r**3 -
                        560 * r**2 + 105 * r
  | n == 5 && m == 1 = 330 * r**5 - 840 * r**4 + 756 * r**3 - 280 * r**2 +
                        35 * r
  | n == 5 && m == 2 = 165 * r**5 - 360 * r**4 + 252 * r**3 - 56 * r**2
  | n == 5 && m == 3 = 55 * r**5 - 90 * r**4 + 36 * r**3
  | n == 5 && m == 4 = 11 * r**5 - 10 * r**4
  | n == 5 && m == 5 = r**5

-- R_nm(r) * e^(ima) = (R_nm(r) * cos (ma), R_nm(r) * sin (ma))
       
zerniker n m r a 
  | r > 1     = 0
  | m < 0     = (zp n (abs m) r) * sin ((abs m)*a)
  | otherwise = (zp n m r) * cos (m*a)

zernikei n m r a
  | r > 1     = 0
  | m < 0     = (zp n (abs m) r) * cos ((abs m)*a)
  | otherwise = (zp n m r) * sin (m*a)

zernikeMoment (n,m) (_,(r,a),v) = 
  ((zerniker n m r a)*v, -(zernikei n m r a)*v)

z = [(0,0),(1,-1),(1,0),(1,1),(2,-2),(2,-1),(2,0),(2,1),(2,2),(3,-3),(3,-2),
     (3,-1),(3,0),(3,1),(3,2),(3,3),(4,-4),(4,-3),(4,-2),(4,-1),(4,0),(4,1),
     (4,2),(4,3),(4,4),(5,-5),(5,-4),(5,-3),(5,-2),(5,-1),(5,0),(5,1),(5,2),
     (5,3),(5,4),(5,5)]

pairSum = foldr1 (\(a1,b1) (a2,b2) -> (a1+a2,b1+b2))

zernikeMoments :: Image GrayScale Float -> [(Float,Float)]
zernikeMoments img = map zm z
  where
    zm (n,m) = sz n $ pairSum $ map (zernikeMoment (n,m)) rp
    sz n (a,b) = ((n+1)/pi*a,(n+1)/pi*b)
    (w,h) = getSize img
    cx = w `div` 2
    cy = h `div` 2
    r = min cx cy
    rp = map (pixelToRadial (cx,cy) r) $ getPixels img

zernikeReconstruct :: (Int,Int) -> [(Float,Float)] -> Image GrayScale Float
zernikeReconstruct (w,h) zs = drawRadial (map (zr (zip z zs)) rp) img
  where
    img = emptyGrayImage (w,h) 0
    zr ps ((x,y),(r,a),_) = ((x,y),(r,a),v')
      where
        v' = sum $ map (zv r a) ps
        zv r a ((n,m),(vr,vi)) = vr*re - vi*im
          where
            re = zerniker n m r a
            im = zernikei n m r a
    cx = w `div` 2
    cy = h `div` 2
    r = min cx cy
    rp = map (pixelToRadial (cx,cy) r) $ getPixels img

drawRadial rp img = unsafePerformIO $ do
  mimg <- toMutable img
  mapM_ (setR mimg) rp
  fromMutable mimg
  where
    setR mimg ((x,y),(r,a),v)
      | r < 1     = setPixel (x,y) v mimg
      | otherwise = setPixel (x,y) 0 mimg

drawRadialRe n m rp img = unsafePerformIO $ do
  mimg <- toMutable img
  mapM_ (setR mimg) rp
  fromMutable mimg
  where
    setR mimg ((x,y),(r,a),v)
      | r < 1     = setPixel (x,y) (zerniker n m r a) mimg -- (r * cos a)
      | otherwise = setPixel (x,y) 0 mimg

drawRadialIm n m rp img = unsafePerformIO $ do
  mimg <- toMutable img
  mapM_ (setR mimg) rp
  fromMutable mimg
  where
    setR mimg ((x,y),(r,a),v)
      | r < 1     = setPixel (x,y) (zernikei n m r a) mimg -- (r * sin a)
      | otherwise = setPixel (x,y) 0 mimg

main = do
  (nbins,inv,inputImage, outputImage) <- readArgs
  img <- readFromFile inputImage
  let
    hist = accHistogram nbins $ getValues img
    t = tOtsu hist
    m | inv = (1,0)
      | otherwise = (0,1)
    timg = threshold m t img
    zm = zernikeMoments timg
    zr = zernikeReconstruct (100,100) zm
    clear = emptyGrayImage (100,100) 1
    rp = map (pixelToRadial (50,50) 50) $ getPixels clear
    r n m = unitNormalize $ drawRadialRe n m rp clear
    i n m = unitNormalize $ drawRadialIm n m rp clear
  saveImage outputImage $ unitNormalize zr
  saveImage "zre.png" $ montage (11,6) 2 $
    [      clear,     clear,     clear,     clear,     clear,(r 0 0),  clear,  clear,  clear,  clear,  clear
    ,      clear,     clear,     clear,     clear,(r 1 (-1)),(r 1 0),(r 1 1),  clear,  clear,  clear,  clear
    ,      clear,     clear,     clear,(r 2 (-2)),(r 2 (-1)),(r 2 0),(r 2 1),(r 2 2),  clear,  clear,  clear
    ,      clear,     clear,(r 3 (-3)),(r 3 (-2)),(r 3 (-1)),(r 3 0),(r 3 1),(r 3 2),(r 3 3),  clear,  clear
    ,      clear,(r 4 (-4)),(r 4 (-3)),(r 4 (-2)),(r 4 (-1)),(r 4 0),(r 4 1),(r 4 2),(r 4 3),(r 4 4),  clear
    , (r 5 (-5)),(r 5 (-4)),(r 5 (-3)),(r 5 (-2)),(r 5 (-1)),(r 5 0),(r 5 1),(r 5 2),(r 5 3),(r 5 4),(r 5 5)
    ]
