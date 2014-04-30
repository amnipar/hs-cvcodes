module Gaussian
( gaussian1D
, gaussian2D
, gaussian2Ddx
, gaussian2Ddy
, gaussian2Ddx2
, gaussian2Ddy2
, gaussian2Ddxdy
, laplacianOfGaussian
, affineGaussian
) where

import BasicUtils

import Debug.Trace

gaussian1D :: Float -> Int -> Float
gaussian1D s x =
  (1 / (sqrt $ 2 * pi * s**2)) * exp (-((iToF x)**2)/(2 * s**2))

gaussian2D :: Float -> (Int,Int) -> Float
gaussian2D s (x,y) =
  (1 / (2 * pi * s**2)) * exp (-(((iToF x)**2 + (iToF y)**2) / (2 * s**2)))

gaussian2Ddx :: Float -> (Int,Int) -> Float
gaussian2Ddx s (x,y) =
  -(-((iToF x) / (2 * pi * s**4))) *
    exp (-(((iToF x)**2 + (iToF y)**2) / (2 * s**2)))

gaussian2Ddy :: Float -> (Int,Int) -> Float
gaussian2Ddy s (x,y) =
  (-((iToF y) / (2 * pi * s**4))) *
    exp(-(((iToF x)**2 + (iToF y)**2) / (2 * s**2)))

gaussian2Ddx2 :: Float -> (Int,Int) -> Float
gaussian2Ddx2 s (x,y) =
  -(-1 + (iToF x)**2/s**2) / (2 * pi * s**4) *
    exp (-(((iToF x)**2 + (iToF y)**2) / (2 * s**2)))

gaussian2Ddy2 :: Float -> (Int,Int) -> Float
gaussian2Ddy2 s (x,y) =
  -(-1 + (iToF y)**2/s**2) / (2 * pi * s**4) *
    exp (-(((iToF x)**2 + (iToF y)**2) / (2 * s**2)))

gaussian2Ddxdy :: Float -> (Int,Int) -> Float
gaussian2Ddxdy s (x,y) =
  ((iToF x) * (iToF y)) / (2 * pi * s**6) *
    exp (-(((iToF x)**2 + (iToF y)**2) / (2 * s**2)))

laplacianOfGaussian :: Float -> (Int,Int) -> Float
laplacianOfGaussian s (x,y) =
  -(1 / (pi * s**4)) * (1 - (((iToF x)**2 + (iToF y)**2) / (2 * s**2))) *
    exp (-(((iToF x)**2 + (iToF y)**2) / (2 * s**2)))

-- | Create an affine Gaussian function by sampling a bivariate Gaussian. For
--   this, will need the covariance matrix of the distribution. This is
--   determined by sigma scaling factors sx and sy and rotation angle t. Need to
--   calculate the inverse and the determinant of this matrix Sigma.
--
--   Covariance matrix Sigma is a matrix product of the diagonal matrix
--     | sx2 0   |
--     | 0   sy2 |
--   and rotation matrix
--     | cos t -sin 2 |
--     | sin t  cos t |
--
--   Sigma = | a = sx2 * cos t  b = -sy2 * sin t |
--           | c = sx2 * sin t  d =  sy2 * cos t |
--
--   Remember the formulas for 2x2 matrix inverse and determinant
--     | a b |-1 = _1_ | d -b |
--     | c d |     det |-c  a |
--
--     det = ad - bc
--
--   Sigma-1 = _1_ | sy * cos t  sy * sin t |
--             det |-sx * sin t  sx * cos t |

affineGaussian :: (Float,Float) -> Float -> Float -> (Int,Int) -> Float
affineGaussian (sx,sy) t s (x,y) =
  (1 / (sqrt $ (2 * pi)**2 * det)) * (exp $ (-1/2) * (x' + y'))
  where
    -- calculate det(Sigma)
    det = sx' * (cos t) * sy' * (cos t) + (sy' * (sin t) * sx' * (sin t))
    idet = 1 / det
    fx = iToF x
    -- need to invert y coordinate
    fy = -iToF y
    -- scaled sigma2
    sx2 = (s * sx)**2
    sy2 = (s * sy)**2
    -- calculate (x' y')T = (x y) Sigma-1 (x y)T
    x' = fx * (idet *   sy'  * (cos t) * fx + idet * sy' * (sin t) * fy)
    y' = fy * (idet * (-sx') * (sin t) * fx + idet * sx' * (cos t) * fy)
