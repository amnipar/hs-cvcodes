module Gaussian
( gaussian1D
, gaussian2D
, gaussian2Ddx
, gaussian2Ddy
, gaussian2Ddx2
, gaussian2Ddy2
, gaussian2Ddxdy
) where

import BasicUtils

gaussian1D :: Float -> Int -> Float
gaussian1D s x =
  (1 / (sqrt $ 2 * pi * s**2)) * exp (-((iToF x)**2)/(2 * s**2))

gaussian2D :: Float -> (Int,Int) -> Float
gaussian2D s (x,y) =
  (1 / (2 * pi * s**2)) * exp(-(((iToF x)**2 + (iToF y)**2) / (2 * s**2)))

gaussian2Ddx :: Float -> (Int,Int) -> Float
gaussian2Ddx s (x,y) =
  (-((iToF x) / (2 * pi * s**4))) * 
    exp (-(((iToF x)**2 + (iToF y)**2) / (2 * s**2)))

gaussian2Ddy :: Float -> (Int,Int) -> Float
gaussian2Ddy s (x,y) = 
  (-((iToF y) / (2 * pi * s**4))) * 
    exp(-(((iToF x)**2 + (iToF y)**2) / (2 * s**2)))

gaussian2Ddx2 :: Float -> (Int,Int) -> Float
gaussian2Ddx2 s (x,y) = 
  (-1 + (iToF x)**2/s**2) / (2 * pi * s**4) * 
    exp (-(((iToF x)**2 + (iToF y)**2) / (2 * s**2)))

gaussian2Ddy2 :: Float -> (Int,Int) -> Float
gaussian2Ddy2 s (x,y) = 
  (-1 + (iToF y)**2/s**2) / (2 * pi * s**4) * 
    exp (-(((iToF x)**2 + (iToF y)**2) / (2 * s**2)))

gaussian2Ddxdy :: Float -> (Int,Int) -> Float
gaussian2Ddxdy s (x,y) = 
  ((iToF x) * (iToF y)) / (2 * pi * s**6) *
    exp (-(((iToF x)**2 + (iToF y)**2) / (2 * s**2)))
