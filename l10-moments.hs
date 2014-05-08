module Main where

import CV.Image
import CV.Filters
import CV.ImageOp
import CV.ImageMathOp
import CV.Drawing
import CV.Operations

import BasicUtils
import DrawingUtils
import Images
import Filters
import Histogram
import Thresholding
import Neighborhoods
import Shapes

import ReadArgs

type RawMoments = (Float,Float,Float,Float,Float,Float,Float,Float,Float,Float)
type CentralMoments = (Float,Float,Float,Float,Float,Float,Float,Float,Float)

objectPixel :: ((Int,Int),Float) -> Bool
objectPixel (_,v) = v == 1

rawMoments :: Image GrayScale Float -> RawMoments
rawMoments image = mom
  where
    op = filter objectPixel $ getPixels image
    mom = foldr accMom (0,0,0,0,0,0,0,0,0,0) op
    accMom ((x,y),v) (m00,m10,m01,m11,m20,m02,m30,m21,m12,m03) =
      (m00',m10',m01',m11',m20',m02',m30',m21',m12',m03')
      where
        fx = iToF x
        fy = iToF y
        m00' = m00 + v
        m10' = m10 + fx * v
        m01' = m01 + fy * v
        m11' = m11 + fx * fy * v
        m20' = m20 + fx**2 * v
        m02' = m02 + fy**2 * v
        m30' = m30 + fx**3 * v
        m21' = m21 + fx**2 * fy * v
        m12' = m12 + fx * fy**2 * v
        m03' = m03 + fy**3 * v

centralMoments :: RawMoments -> CentralMoments
centralMoments (m00,m10,m01,m11,m20,m02,m30,m21,m12,m03) =
  (cx,cy,c11,c20,c02,c30,c20,c12,c03)
  where
    cx = m10/m00
    cy = m01/m00
    c11 = m11 - cx*m01
    c20 = m20 - cx*m10
    c02 = m02 - cy*m01
    c30 = m30 - 3 * cx * m20 + 2 * cx**2 * m10
    c21 = m21 - 2 * cx * m11 - cy * m20 + 2 * cx**2 * m01
    c12 = m12 - 2 * cy * m11 - cx * m02 + 2 * cy**2 * m10
    c03 = m03 - 3 * cy * m02 + 2 * cy**2 * m01

ellipsoid :: RawMoments -> ((Int,Int),(Int,Int),Float)
ellipsoid (m00,m10,m01,m11,m20,m02,_,_,_,_) =
  ((round cx, round cy),(round r1, round r2),180*a)
  where
      cx = m10 / m00
      cy = m01 / m00
      c20 = m20 / m00 - cx**2
      c11 = m11 / m00 - cx*cy
      c02 = m02 / m00 - cy**2
      l1 :: Float
      l1 = 0.5 * (c20 - c02) + 0.5 * (sqrt $ 4 * c11**2 + (c20 - c02)**2)
      l2 :: Float
      l2 = 0.5 * (c20 - c02) - 0.5 * (sqrt $ 4 * c11**2 + (c20 - c02)**2)
      e = sqrt $ 1 - l2 / l1
      r = 1 - l2 / l1
      r1 = sqrt $ m00 / (pi * r)
      r2 = r * r1
      a :: Float
      a = 0.5 * (atan2 (-2 * c11) (c20 - c02))

drawEllipsoid :: ((Int,Int),(Int,Int),Float) -> Image RGB Float
    -> Image RGB Float
drawEllipsoid (p,r,a) image =
  image
    <# ellipseOp red 1 p r a (0,360)
    <# circleOp red p 3 (Stroked 1)

main = do
  (nbins,inv,inputImage, outputImage) <- readArgs
  img <- readFromFile inputImage
  let
    gimg = gaussian (5,5) img
    hist = accHistogram nbins $ getValues gimg
    t = tOtsu hist
    m | inv = (1,0)
      | otherwise = (0,1)
    timg = threshold m t gimg
    r = rawMoments timg
    c = centralMoments r
    e = ellipsoid r
  saveImage outputImage $ drawEllipsoid e $ convGrayToColor timg
