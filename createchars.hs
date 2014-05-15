module Main where

import CV.Image
import CV.Transforms hiding (rotate)
import CV.ImageMathOp
import CV.Operations

import BasicUtils
import Images
import Random

import ReadArgs
import System.IO.Unsafe
import Debug.Trace
import Control.Monad
import GSL.Random.Gen hiding (getSize)
import GSL.Random.Dist
import Data.Time.Clock
import Foreign(Word64)
import Text.Printf
import Data.Char

rotate :: Double -> (Double,Double) -> (Double,Double) -> (Double,Double)
rotate a (dx,dy) (x,y) =
  (dx + (cos a * (x-dx) - sin a * (y-dy)), dy + (sin a * (x-dx) + cos a * (y-dy)))

squish :: Double -> (Double,Double) -> (Double,Double) -> (Double,Double)
squish s (dx,dy) (x,y) = (dx+s*(x-dx), dy+(1/s)*(y-dy))

translate :: (Double,Double) -> (Double,Double) -> (Double,Double)
translate (dx,dy) (x,y) = (x+dx, y+dy)

bothr (x,y) = (realToFrac x, realToFrac y)

randomPerturbation :: RNG -> Image GrayScale Float -> IO (Image GrayScale Float)
randomPerturbation rng img = do
  let
    (w,h) = getSize img
    (cx,cy) = ((fromIntegral w) / 2, (fromIntegral h) / 2)
    sw = 0.03 * (fromIntegral w)
    sh = 0.03 * (fromIntegral h)
    xs :: [Double]
    xs = [0,fromIntegral w,fromIntegral w,0]
    ys :: [Double]
    ys = [0,0,fromIntegral h,fromIntegral h]
    ps = zip xs ys
    ps' = corruptPairsWithGaussian (realToFrac sw) (map bothr ps)
  a <- getGaussian rng (pi/32)
  s <- getGaussian rng 0.03
  tx <- getGaussian rng sw
  ty <- getGaussian rng sh
  let
    img' = perspectiveTransform img $ getHomography ps $
      map ((rotate a (cx,cy)).(squish (1+s) (cx,cy))) $ map bothr ps' -- (translate (tx,ty)).
    (w',h') = getSize img'
    (cx',cy') = (w' `div` 2, h' `div` 2)
    (w2,h2) = (round $ 0.4 * fromIntegral w', round $ 0.4 * fromIntegral h')
  return $ getRegion (cx'-w2,cy'-h2) (w2*2,h2*2) img'

numImages = 99

writeImage :: String -> (Int,Image GrayScale Float) -> IO ()
writeImage base (i,img) = saveImage (printf (base ++ "_%02d.png") i) img

createCharImages :: RNG -> Int -> Char -> IO ()
createCharImages rng n c = do
  let
    cu = toUpper c
    cl = toLower c
  print cu
  img <- readFromFile $ "char/" ++ [cu] ++ "u.png"
  imgs <- replicateM n $ randomPerturbation rng img
  mapM_ (writeImage ("./dchars/" ++ [cl] ++ "/" ++ [cl])) $ zip [1..] imgs

chars = "abcdefghijklmnopqrstuvwxyz"

main = do
  (inputImage,outputImage) <- readArgs
  img <- readFromFile inputImage
  time <- getCurrentTime >>= return . utctDayTime
  rng <- newRNG mt19937
  setSeed rng $ (floor.(*1000000).toRational) time
  c <- replicateM 16 $ randomPerturbation rng img
  c' <- replicateM 1000 $ randomPerturbation rng img
  saveImage outputImage $ montage (4,4) 2 c
  --let
  --  clear = emptyGrayImage (21,21) 0
  --saveImage "a.png" $ unitNormalize $ foldr (#+) clear $ map (resizeImage (21,21)) c'
  mapM_ (createCharImages rng numImages) chars
