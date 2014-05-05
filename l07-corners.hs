module Main where

import CV.Image
import CV.Filters
import qualified CV.ImageMath as IM hiding (not)
import CV.ImageMathOp
import CV.ColourUtils
import CV.Pixelwise
import CV.Drawing
import CV.ImageOp
import CV.Operations

import BasicUtils
import DrawingUtils
import Images
import Filters
import Gaussian
import Neighborhoods
import Thresholding

import ReadArgs
import Control.Monad

harrisResponse :: Image GrayScale D32 -> Image GrayScale D32
harrisResponse image = stretchHistogram $ det #- (kappa |* (tra #* tra))
  where
    det = (a11 #* a22) #- (a12 #* a21)
    tra = a11 #+ a22
    dx = derivativex image
    dy = derivativey image
    a11 = integrate $ dx #* dx
    a22 = integrate $ dy #* dy
    a12 = integrate $ dx #* dy
    a21 = a12
    kappa = 0.15

-- Uses the Harris second moment matrix to get the ellipse corresponding to
-- the affine region. This can be done by calculating the eigenvalues and
-- eigenvectors of the square root of the matrix.
--
-- Remembering the formulas for eigenvalues and eigenvectors of 2x2 matrix:
--      | a   b | a11 a12
-- A =  |       |
--      | c   d | a21 a22
--
-- T=a+d D=ad-bc
--
-- L1 = T/2 + (T2/4-D)1/2
-- L2 = T/2 - (T2/4-D)1/2
--
-- V1 = | L1 - d | V2 = | L2 - d | if c=0, then V1 = | 1 | V2 = | 0 |
--      |    c   |      |    c   |                   | 0 |      | 1 |
--
harrisToEllipse ::
  (Image GrayScale Float, Image GrayScale Float, Image GrayScale Float)
  -> Int -> (Int,Int) -> ((Int,Int),(Int,Int),Float)
harrisToEllipse (dx2,dy2,dxdy) s (x,y) = ((x,y),(r1,r2),ang)
  where
    a = getPixel (x,y) dx2
    b = getPixel (x,y) dxdy
    c = b
    d = getPixel (x,y) dy2
    tra = a + d
    det = a * d - b * c
    base = tra / 2
    mod = sqrt $ tra**2 / 4 - det
    -- the ellipse shape is determined by square roots of eigenvalues
    l1 = sqrt $ base + mod
    l2 = sqrt $ base - mod
    -- the 'roundness' of the ellipse is determined by the ratio of eigenvalues
    r1 | l1 > l2   = round $ (iToF s) * (l1 / l2)
       | otherwise = round $ (iToF s) * (l2 / l1)
    r2 = s
    -- OpenCV takes the angle in degrees
    ang | (abs c) < 0.0001 = 90
        | l1 > l2          = 180 * (atan2 c l1 - (sqrt d))
        | otherwise        = 180 * (atan2 c l2 - (sqrt d))

-- Calculates the eigenvalues of the Harris second moment matrix and returns the
-- smaller one of them for each pixel. This is the Shi-Tomasi cornerness
-- measure.
--
-- Remembering the formulas for 2x2 matrix eigenvalues:
--      | a   b | a11 a12
-- A =  |       |
--      | c   d | a21 a22
--
-- T=a+d D=ad-bc
--
-- L1 = T/2 + (T2/4-D)1/2
-- L2 = T/2 - (T2/4-D)1/2

harrisLambda :: Image GrayScale D32 -> Image GrayScale D32
harrisLambda image = stretchHistogram $ IM.min lambda1 lambda2
  where
    det = (a11 #* a22) #- (a12 #* a21)
    tra = a11 #+ a22
    htra = 0.5 |* tra
    stra24 = IM.sqrt $ (0.25 |* (tra #* tra)) #- det
    lambda1 = htra #+ stra24
    lambda2 = htra #- stra24
    dx = derivativex image
    dy = derivativey image
    a11 = integrate $ dx #* dx
    a22 = integrate $ dy #* dy
    a12 = integrate $ dx #* dy
    a21 = a12

harrisLambdas :: Image GrayScale D32 -> (Image GrayScale D32,Image GrayScale D32)
harrisLambdas image = (lambda1,lambda2)
  where
    det = (a11 #* a22) #- (a12 #* a21)
    tra = a11 #+ a22
    htra = 0.5 |* tra
    stra24 = IM.sqrt $ (0.25 |* (tra #* tra)) #- det
    lambda1 = htra #+ stra24
    lambda2 = htra #- stra24
    dx = derivativex image
    dy = derivativey image
    a11 = integrate $ dx #* dx
    a22 = integrate $ dy #* dy
    a12 = integrate $ dx #* dy
    a21 = a12

-- Hessian matrix:
-- | dx2  dxdy |
-- | dxdy dy2  |
--
-- T=dx2+dy2       = LoG
-- D=dx2*dy2-dxdy2 = DoH

hessian :: Image GrayScale D32 -> (Image GrayScale D32,Image GrayScale D32)
hessian image = (idoh,ilog)
  where
    dx2 = derivativex2 image
    dy2 = derivativey2 image
    dxdy = derivativexy image
    idoh = stretchHistogram $ (dx2 #* dy2) #- (dxdy #* dxdy)
    ilog = stretchHistogram $ IM.sqrt $ (dx2 #+ dy2) #* (dx2 #+ dy2)

-- L1 = T/2 + (T2/4-D)1/2
-- L2 = T/2 - (T2/4-D)1/2

-- get the eigenvalues of Hessian as images
hessianLambdas :: Image GrayScale D32 -> (Image GrayScale D32,Image GrayScale D32)
hessianLambdas image = (lambda1,lambda2)
  where
    dx2 = derivativex2 image
    dy2 = derivativey2 image
    dxdy = derivativexy image
    det = (dx2 #* dy2) #- (dxdy #* dxdy)
    tra = dx2 #+ dy2
    htra = 0.5 |* tra
    stra24 = IM.sqrt $ (0.25 |* (tra #* tra)) #- det
    lambda1 = htra #+ stra24
    lambda2 = htra #- stra24

logThreshold :: Float -> Image GrayScale Float -> Image GrayScale Float
logThreshold t image = threshold (0,1) t log
  where
    log = IM.sqrt $ (dx2 #+ dy2) #* (dx2 #+ dy2)
    dx2 = (dsigma**2) |* derivativex2 image
    dy2 = (dsigma**2) |* derivativey2 image

hessianThreshold :: Float -> Image GrayScale Float -> Image GrayScale Float
hessianThreshold r image = threshold (0,1) t ratio
  where
    dx2 = derivativex2 image
    dy2 = derivativey2 image
    dxdy = derivativexy image
    det = (dx2 #* dy2) #- (dxdy #* dxdy)
    tra = dx2 #+ dy2
    ratio = IM.div det (IM.maxS 0.0001 $ tra #* tra)
    t = r / (r+1)**2

maximalHarris :: Image GrayScale D32 -> Image RGB D32
maximalHarris image =
  plotEllipses red 1 
    (map ((harrisToEllipse (a,d,c) (isize `div` 2)).fst) points) $ 
      plotPoints red 3 (map fst points) $
        grayToRGB $ zeroNormalize $ dx #+ dy
  where
    dx = derivativex image
    dy = derivativey image
    a = integrate $ dx #* dx
    d = integrate $ dy #* dy
    c = integrate $ dx #* dy
    points = relativeThresholdPoints 0.1 $ filterNeighborhood n8 isMaximal $
        harrisLambda image

circleToEllipse (p,r) = (p,(r,2*r),45)

isMaximal (v,ns) = v > (maximum ns)

maximalHessian :: Image GrayScale D32 -> Image RGB D32
maximalHessian kuva =
  plotEllipses red 0 (map circleToEllipse $ pointsToCircles 5 points) $ grayToRGB kuva
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
    dx = derivativex kuva
    dy = derivativey kuva
    a11 = integrate $ dx #* dx
    a22 = integrate $ dy #* dy
    a12 = integrate $ dx #* dy
    a21 = integrate $ dy #* dx
    kappa = 0.10
    r = ((a11 #* a22) #- (a12 #* a21)) #- (kappa |* ((a11 #+ a22) #* (a11 #+ a22)))

integrate = convolve2D maskg icenter
derivativex = convolve2D maskdx dcenter
derivativey = convolve2D maskdy dcenter
derivativex2 = convolve2D maskdx2 dcenter
derivativey2 = convolve2D maskdy2 dcenter
derivativexy = convolve2D maskdxdy dcenter

isigma = 1.6
dsigma = 0.5 * isigma
-- mask should fit 6 sigma
dsize = 2 * (ceiling $ 3 * dsigma) + 1
isize = 2 * (ceiling $ 3 * isigma) + 1
dcenter = getMaskCenter2D dsize
icenter = getMaskCenter2D isize
maskg = createMask2D (gaussian2D isigma) dsize
maskdx = createMask2D (gaussian2Ddx dsigma) dsize
maskdy = createMask2D (gaussian2Ddy dsigma) dsize
maskdx2 = createMask2D (gaussian2Ddx2 dsigma) dsize
maskdy2 = createMask2D (gaussian2Ddy2 dsigma) dsize
maskdxdy = createMask2D (gaussian2Ddxdy dsigma) dsize

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
    "harrisl" ->
      saveImage targetImage $ montage (2,1) 2 $ map stretchHistogram $
         pairToList $ harrisLambdas img
    "harris" ->
      saveImage targetImage $ maximalHarris img
    "hessian" ->
      saveImage targetImage $ maximalHessian img
    "hessianl" ->
      saveImage targetImage $ montage (2,1) 2 $ map unitNormalize $ pairToList $
          hessianLambdas img
    "hessiant" ->
      saveImage targetImage $ hessianThreshold 2 img
    "logt" ->
      saveImage targetImage $ logThreshold 0.05 img
    "rhessian" ->
      saveImage targetImage $ montage (2,1) 2 $ pairToList $ hessian img
    "affine" ->
      saveImage targetImage $ montage (2,1) 2 $
        [ stretchHistogram $ drawFilter 200 (gaussian2D (200/12))
        , stretchHistogram $ drawFilter 200 (affineGaussian (2,1) (pi/8) (200/12)) ]
    otherwise -> error usage

-- (Color a b) -> Int -> (Int,Int) -> (Int,Int) -> Float -> (Float,Float)
-- ellipseOp c t (x,y) (r1,r2) a (a1,a2)