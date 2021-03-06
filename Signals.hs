module Signals
( generateSignal
, sample
, generateFrequencyComponent
) where

import BasicUtils

generateFrequencyComponent :: [Float] -> [Float] -> Int -> Float -> Float
generateFrequencyComponent ms ps i x
  | length ms < (i+1) || length ps < length ms || i < 0 = 0
  | i == 0 = head ms
  | otherwise = (ms !! i) * sin ((iToF i) * x + (ps !! i)*pi)

generateSignal :: [Float] -> [Float] -> Float -> Float
generateSignal (m:ms) (p:ps) x =
  m + (sum $ map (s x) $ zip3 ms ps [1..10])
  where
    s :: Float -> (Float,Float,Float) -> Float
    s x (a,p,i) = a * sin (i*x+p*pi)

sample :: Int -> Float -> (Float -> Float) -> [(Float,Float)]
sample n scale f = map (s f) domain
  where
        s f x = (x, f x)
        domain = [scale * (((iToF x) - (iToF n / 2)) / (iToF n)) | x <- [0..n]]
