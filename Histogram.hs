module Histogram
( accHistogram
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
