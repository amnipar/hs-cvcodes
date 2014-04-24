module Main where

import CV.Image
import CV.Filters
import CV.ImageMath as IM hiding (not)
import CV.ImageMathOp
import CV.ColourUtils
import CV.Pixelwise
import CV.Drawing
import CV.ImageOp
import CV.Operations

import DrawingUtils
import Filters
import Gaussian
import Neighborhoods
import Thresholding

import ReadArgs
import Control.Monad

--filterPixels :: (Float -> Bool) -> Image GrayScale D32 -> [((Int,Int),Float)]
--filterPixels cond img = filter (cond . snd) $
--    [ ((i,j),(v i j)) | j<-[0..height-1] , i <- [0..width-1] ]
--  where
--    v x y = getPixel (x,y) img
--    (width,height) = getSize img

isMaximal (v,ns) = v > (maximum ns)

--      | a   b | a11 a12
-- A =  |       |
--      | c   d | a21 a22

-- T=a+d D=ad-bc

-- L1 = T/2 + (T2/4-D)1/2
-- L2 = T/2 - (T2/4-D)1/2

harrisResponse :: Image GrayScale D32 -> Image GrayScale D32
harrisResponse image = stretchHistogram $ det #- (kappa |* (tra #* tra))
  where
    det = (a11 #* a22) #- (a12 #* a21)
    tra = a11 #+ a22
    dx = convolve2D maskdx center image
    dy = convolve2D maskdy center image
    a11 = gaussian (5,5) $ dx #* dx
    a22 = gaussian (5,5) $ dy #* dy
    a12 = gaussian (5,5) $ dx #* dy
    a21 = a12
    kappa = 0.15

harrisLambda :: Image GrayScale D32 -> Image GrayScale D32
harrisLambda image = stretchHistogram $ IM.min lambda1 lambda2
  where
    det = (a11 #* a22) #- (a12 #* a21)
    tra = a11 #+ a22
    htra = 0.5 |* tra
    stra24 = IM.sqrt $ (0.25 |* (tra #* tra)) #- det
    lambda1 = htra #+ stra24
    lambda2 = htra #- stra24
    dx = convolve2D maskdx center image
    dy = convolve2D maskdy center image
    a11 = gaussian (5,5) $ dx #* dx
    a22 = gaussian (5,5) $ dy #* dy
    a12 = gaussian (5,5) $ dx #* dy
    a21 = a12

hessian :: Image GrayScale D32 -> (Image GrayScale D32,Image GrayScale D32)
hessian image = (idoh,ilog)
  where
    dx2 = convolve2D maskdx2 center image
    dy2 = convolve2D maskdy2 center image
    dxdy = convolve2D maskdxdy center image
    idoh = stretchHistogram $ (dx2 #* dy2) #- (dxdy #* dxdy)
    ilog = stretchHistogram $ IM.sqrt $ (dx2 #+ dy2) #* (dx2 #+ dy2)

maximalHarris :: Image GrayScale D32 -> Image RGB D32
maximalHarris kuva =
  plotCircles red 0 (pointsToCircles 5 points) $ grayToRGB kuva
  --grayToRGB kuva
  --  <## [circleOp (0,0,1) (x,y) 5 Filled | ((x,y),v) <- kulmat]
  where
    points = relativeThresholdPoints 0.1 $ filterNeighborhood n8 isMaximal $
        harrisLambda kuva

maximalHessian :: Image GrayScale D32 -> Image RGB D32
maximalHessian kuva =
  plotCircles red 0 (pointsToCircles 5 points) $ grayToRGB kuva
  where
    points = relativeThresholdPoints2 0.4 $ filterNeighborhood2 n8 isMaximal $
        hessian kuva

harriskulmat :: Image GrayScale D32 -> Image GrayScale D32
harriskulmat kuva = montage (2,3) 4
  [ -- stretchHistogram $ harris 5 5 0.04 kuva
    stretchHistogram $ r
  , stretchHistogram $ a11
  , stretchHistogram $ a12
  , stretchHistogram $ a21
  , stretchHistogram $ a22
  ]
  where
    c :: (Int,Int)
    c = (2,2)
    dx = convolve2D maskdx center kuva
    dy = convolve2D maskdy center kuva
    a11 = gaussian (5,5) $ dx #* dx
    a22 = gaussian (5,5) $ dy #* dy
    a12 = gaussian (5,5) $ dx #* dy
    a21 = gaussian (5,5) $ dy #* dx
    kappa = 0.10
    r = ((a11 #* a22) #- (a12 #* a21)) #- (kappa |* ((a11 #+ a22) #* (a11 #+ a22)))

sigma = 1
-- mask should fit 6 sigma
size = 7
center = getMaskCenter2D size
maskdx = createMask2D (gaussian2Ddx sigma) size
maskdy = createMask2D (gaussian2Ddy sigma) size
maskdx2 = createMask2D (gaussian2Ddx2 sigma) size
maskdy2 = createMask2D (gaussian2Ddy2 sigma) size
maskdxdy = createMask2D (gaussian2Ddxdy sigma) size

usage :: String
usage = "usage: l07-corners [harris|hessian] source target"

pairToList (a,b) = [a,b]

main = do
  (mode,sourceImage,targetImage) <- readArgs
  img <- readFromFile sourceImage
  case mode of
    "rharris" ->
      saveImage targetImage $ harrisResponse img
    "lharris" ->
      saveImage targetImage $ harrisLambda img
    "harris" ->
      saveImage targetImage $ maximalHarris img
    "hessian" ->
      saveImage targetImage $ maximalHessian img
    "rhessian" ->
      saveImage targetImage $ montage (2,1) 2 $ pairToList $ hessian img
    otherwise -> error usage
