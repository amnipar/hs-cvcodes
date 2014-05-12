module Shapes
( generateTriangle
, generateTriangleImage
, generateTriangleImages
, generateQuad
, generateQuadImage
, generateQuadImages
, generateCircle
, generateCircleImage
, generateCircleImages
) where

import CV.Image
import CV.ImageMath hiding (div)
import CV.ImageMathOp
import CV.ImageOp
import CV.Drawing
import CV.Pixelwise

import Control.Monad
import System.IO.Unsafe
import System.Random
import GSL.Random.Gen hiding (getSize)
import GSL.Random.Dist

import BasicUtils
import Random

rotate :: Double -> (Int,Int) -> (Int,Int)
rotate a (x,y) =
  (round $ cos a * fx - sin a * fy, round $ sin a * fx + cos a * fy)
  where
    fx = fromIntegral x
    fy = fromIntegral y

squish :: Double -> (Int,Int) -> (Int,Int)
squish s (x,y) = (round $ s*(fromIntegral x), round $ (1/s)*(fromIntegral y))

translate :: (Int,Int) -> (Int,Int) -> (Int,Int)
translate (x,y) (dx,dy) = (x+dx, y+dy)

generateTriangle :: RNG -> Float -> (Int,Int) -> IO [(Int,Int)]
generateTriangle rng sigma (w,h) = do
  let
    (cx,cy) = (w `div` 2, h `div` 2)
    sw = 0.2 * (iToF w)
    sh = 0.2 * (iToF h)
    ca = 2 * pi / 3
    xs = [ 0,-(sin (-ca)) * sh, -(sin ca) * sh]
    ys = [sh, (cos ca) * sh,     (cos ca) * sh]
  xs' <- corruptWithGaussian rng (sigma*sw) xs
  ys' <- corruptWithGaussian rng (sigma*sh) ys
  a <- getFlat rng (-pi) pi
  s <- getGaussian rng 0.2
  return $ map ((translate (cx,cy)).(rotate a).(squish (1+s))) $
      zip (map round xs') (map round ys')

generateQuad :: RNG -> Float -> (Int,Int) -> IO [(Int,Int)]
generateQuad rng sigma (w,h) = do
  let
    (cx,cy) = (w `div` 2, h `div` 2)
    sw = 0.2 * (iToF w)
    sh = 0.2 * (iToF h)
    xs = [-sw,-sw,sw,sw]
    ys = [-sh,sh,sh,-sh]
  xs' <- corruptWithGaussian rng (sigma*sw) xs
  ys' <- corruptWithGaussian rng (sigma*sh) ys
  a <- getFlat rng (-pi) pi
  s <- getGaussian rng 0.2
  return $ map ((translate (cx,cy)).(rotate a).(squish (1+s))) $
      zip (map round xs') (map round ys')

generateCircle :: RNG -> Float -> (Int,Int) -> IO ((Int,Int),(Int,Int),Float)
generateCircle rng sigma (w,h) = do
  a <- getFlat rng (-pi) pi
  s <- getGaussian rng 0.2
  let
    (cx,cy) = (w `div` 2, h `div` 2)
    s' = realToFrac $ 1+s
    sw = round $ 0.2 * s' * (iToF w)
    sh = round $ 0.2 * (1/s') * (iToF h)
  return ((cx,cy),(sw,sh),realToFrac a)

drawPoly size corners =
  fillPoly 0.5 corners $ 0.1 |+ blankImageGray (size,size)

drawCircle size ((x,y),(w,h),a) =
  image <# ellipseOp 0.5 (-1) (x,y) (w,h) (180*a) (0,360)
  where
    image = 0.1 |+ blankImageGray (size,size)

generateTriangleImage :: Int -> Image GrayScale Float
generateTriangleImage size = unsafePerformIO $ do
  rng <- newRNG mt19937
  setSeed rng 12349871235125
  sq <- generateTriangle rng 0.1 (size,size)
  drawPoly size sq

generateTriangleImages :: Int -> Int -> [Image GrayScale Float]
generateTriangleImages n size = unsafePerformIO $ do
  rng <- newRNG mt19937
  setSeed rng 12349871235125
  sq <- replicateM n $ generateTriangle rng 0.1 (size,size)
  mapM (drawPoly size) sq


generateQuadImage :: Int -> Image GrayScale Float
generateQuadImage size = unsafePerformIO $ do
  rng <- newRNG mt19937
  setSeed rng 12349871235125
  sq <- generateQuad rng 0.1 (size,size)
  drawPoly size sq

generateQuadImages :: Int -> Int -> [Image GrayScale Float]
generateQuadImages n size = unsafePerformIO $ do
  rng <- newRNG mt19937
  setSeed rng 12349871235125
  sq <- replicateM n $ generateQuad rng 0.1 (size,size)
  mapM (drawPoly size) sq

generateCircleImage :: Int -> Image GrayScale Float
generateCircleImage size = unsafePerformIO $ do
  rng <- newRNG mt19937
  setSeed rng 12349871235125
  c <- generateCircle rng 0.1 (size,size)
  return $ drawCircle size c

generateCircleImages :: Int -> Int -> [Image GrayScale Float]
generateCircleImages n size = unsafePerformIO $ do
  rng <- newRNG mt19937
  setSeed rng 12349871235125
  c <- replicateM n $ generateCircle rng 0.1 (size,size)
  return $ map (drawCircle size) c

--gaussianRandom ::
--(v,r) = splitAt 10 rand

--corruptGaussian :: RNG -> Float -> Image GrayScale D32 -> (Int,Int)
--    -> Image GrayScale D32

additiveGaussian rng sigma f (x,y) = unsafePerformIO $ do
  g <- getGaussian rng sigma
  return $ f (x,y) + (realToFrac g)

addGaussianNoise :: RNG -> Double -> Image GrayScale D32
    -> Image GrayScale D32
addGaussianNoise rng sigma image =
  remapImage (additiveGaussian rng sigma) image

blankImageGray :: (Int,Int) -> Image GrayScale D32
blankImageGray (w,h) = empty (w,h)
{-
gradientImage :: (Int,Int) -> Double -> IO (Image GrayScale D32)
gradientImage (w,h) a = do
  img <- readFromFile "gradient-big.png"
  let
    rot = rotate a img
    (rw,rh) = getSize rot
    (w2,h2) = (w `div` 2, h `div` 2)
    (cx,cy) = (rw `div` 2, rh `div` 2)
  return $ getRegion (cx-w2,cy-h2) (w,h) rot

rampImage :: (Int,Int) -> Double -> IO (Image GrayScale D32)
rampImage (w,h) a = do
  img <- readFromFile "ramp-big.png"
  let
    rot = rotate a img
    (rw,rh) = getSize rot
    (w2,h2) = (w `div` 2, h `div` 2)
    (cx,cy) = (rw `div` 2, rh `div` 2)
  return $ getRegion (cx-w2,cy-h2) (w,h) rot
-}