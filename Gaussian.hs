module Gaussian
( gaussian1D
, gaussian2D
) where

import BasicUtils

gaussian1D :: Float -> Int -> Float
gaussian1D s x =
  (1 / (sqrt $ 2 * pi * s**2)) * exp (-((iToF x)**2)/(2 * s**2))

gaussian2D :: Float -> (Int,Int) -> Float
gaussian2D s (x,y) =
  (1 / (2 * pi * s**2)) * exp(-(((iToF x)**2 + (iToF y)**2) / (2 * s**2)))
