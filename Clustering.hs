{-# LANGUAGE ParallelListComp #-}
module Clustering
( clusterGrayValue
, clusterGraySpatial
) where

import CV.Image

import Math.KMeans
import System.IO.Unsafe
import Control.Monad
import qualified Data.Vector.Unboxed as V

import BasicUtils

makeGrays :: Int -> [Float]
makeGrays n = [(iToF i)/(iToF $ n-1) | i <- [0..n-1]]

setPixelsGray :: MutableImage GrayScale Float -> [(Int,Int)] -> Float -> IO ()
setPixelsGray m xs v = forM_ xs $ \c -> setPixel c v m

renderClustersGray :: (Int,Int) -> [[(Int,Int)]] -> Image GrayScale Float
renderClustersGray size points = unsafePerformIO $ do
  mimg <- toMutable $ empty size
  sequence_
    [ setPixelsGray mimg pts gray
    | pts <- points
    | gray <- makeGrays (length points)
    ]
  fromMutable mimg

coordinates = map (map snd)

grayValueFeature :: Image GrayScale Float -> [Point (Int,Int)]
grayValueFeature image =
  [ (V.fromList [realToFrac $ getPixel (i,j) image], (i,j))
  | i <- [0,1..w-1], j <- [0,1..h-1]]
  where (w,h) = getSize image

graySpatialFeature :: Double -> Image GrayScale Float -> [Point (Int,Int)]
graySpatialFeature factor image =
  [ (V.fromList [scale i, scale j] V.++
     V.fromList [realToFrac $ getPixel (i,j) image], (i,j))
  | i <- [0,1..w-1], j <- [0,1..h-1]]
  where
    (w,h) = getSize image
    s = realToFrac $ max w h
    scale x = factor * ((realToFrac x) / s)

clusterGrayValue :: Int -> Image GrayScale Float -> Image GrayScale Float
clusterGrayValue k image = renderClustersGray (getSize image) $ coordinates $
    kmeans k $ grayValueFeature image

clusterGraySpatial :: Int -> Double -> Image GrayScale Float
    -> Image GrayScale Float
clusterGraySpatial k f image = renderClustersGray (getSize image) $ coordinates $
  kmeans k $ graySpatialFeature f image
