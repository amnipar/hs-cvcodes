module Histogram
( accHistogram
, accVectorHistogram
, accBoundedVectorHistogram
) where

import BasicUtils

import qualified Data.List as List
import qualified Data.Map as Map

-- | Accumulate a histogram of values by dividing the range into a number of
--   equally wide bins and using a Map to accumulate the bin values. The
--   histogram is normalized into a probability mass function.
accHistogram :: Int -> [Float] -> [(Float,Float)]
accHistogram bins xs = norm $ Map.toList $ List.foldl' accBin initmap xs
  where
    xmin = minimum xs
    xmax = maximum xs
    xrange = xmax - xmin
    xstep = xrange / (iToF bins)
    initmap :: Map.Map Int Float
    initmap = List.foldl' (\m k -> Map.insert k 0 m) Map.empty [0..bins-1]
    toIdx x = floor $ (x - xmin) / xstep
    accBin :: Map.Map Int Float -> Float -> Map.Map Int Float
    accBin m x = Map.insertWith' (+) (toIdx x) 1 m
    norm bs = map (norm' s) bs
      where
        s = sum $ map snd bs
        norm' s (l,c) = (xmin + (iToF l) * xstep, c/s)

accVectorHistogram :: Int -> [[Float]] -> [[(Float,Float)]]
accVectorHistogram bins xss =
  norm $ map Map.toList $ List.foldl' accBins initMaps xss
  where
    n = length xss
    xmins = map minimum xss
    xmaxs = map maximum xss
    xranges = zipWith (-) xmaxs xmins
    xsteps = map (/(iToF bins)) xranges
    initMaps :: [Map.Map Int Float]
    initMaps = replicate n $
      List.foldl' (\m k -> Map.insert k 0 m) Map.empty [0..bins-1]
    toIdx i x = floor $ (x - (xmins !! i)) / (xsteps !! i)
    accBins :: [Map.Map Int Float] -> [Float] -> [Map.Map Int Float]
    accBins ms xs = map accBin $ zip3 [0..] ms xs
    accBin (i,m,x) = Map.insertWith' (+) (toIdx i x) 1 m
    norm :: [[(Int,Float)]] -> [[(Float,Float)]]
    norm bss = map norm' $ zip3 bss xmins xsteps
      where
        norm' (bs,xmin,xstep) = map (norm'' s) bs
          where
            s = sum $ map snd bs
            norm'' s (l,c) = (xmin + (iToF l) * xstep, c/s)


accBoundedVectorHistogram :: ([Float],[Float]) -> Int -> [[Float]]
    -> [[(Float,Float)]]
accBoundedVectorHistogram (xmins,xmaxs) bins xss =
  norm $ map Map.toList $ List.foldl' accBins initMaps xss
  where
    n = length xss
    --xmins = map minimum xss
    --xmaxs = map maximum xss
    xranges = zipWith (-) xmaxs xmins
    xsteps = map (/(iToF bins)) xranges
    initMaps :: [Map.Map Int Float]
    initMaps = replicate n $
      List.foldl' (\m k -> Map.insert k 0 m) Map.empty [0..bins-1]
    toIdx i x = floor $ (x - (xmins !! i)) / (xsteps !! i)
    accBins :: [Map.Map Int Float] -> [Float] -> [Map.Map Int Float]
    accBins ms xs = map accBin $ zip3 [0..] ms xs
    accBin (i,m,x) = Map.insertWith' (+) (toIdx i x) 1 m
    norm :: [[(Int,Float)]] -> [[(Float,Float)]]
    norm bss = map norm' $ zip3 bss xmins xsteps
      where
        norm' (bs,xmin,xstep) = map (norm'' s) bs
          where
            s = sum $ map snd bs
            norm'' s (l,c) = (xmin + (iToF l) * xstep, c/s)
