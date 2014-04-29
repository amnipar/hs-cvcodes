module Main where

import CV.Image
import CV.ImageMath as IM hiding (div)
import CV.ImageMathOp
import CV.Filters
import CV.Pixelwise
import CV.ColourUtils
import CV.Operations

import DrawingUtils
import Filters
import Gaussian
import Thresholding
import Neighborhoods

import ReadArgs

sigma = 0.8
-- mask should fit 6 sigma
size = 5
center = getMaskCenter2D size
maskg = createMask2D (gaussian2D sigma) size
maskdx = createMask2D (gaussian2Ddx sigma) size
maskdy = createMask2D (gaussian2Ddy sigma) size
maskdx2 = createMask2D (gaussian2Ddx2 sigma) size
maskdy2 = createMask2D (gaussian2Ddy2 sigma) size
maskdxdy = createMask2D (gaussian2Ddxdy sigma) size
masklog = createMask2D (laplacianOfGaussian sigma) size

imageDerivatives1 image =
  [ stretchHistogram dx
  , stretchHistogram dy ]
  where
    dx = convolve2D maskdx center image
    dy = convolve2D maskdy center image

imageDerivatives2 image =
  [ stretchHistogram dx2
  , stretchHistogram dy2
  , stretchHistogram dxdy
  , stretchHistogram ilog
  ]
  where
    dx2 = convolve2D maskdx2 center image
    dy2 = convolve2D maskdy2 center image
    dxdy = convolve2D maskdxdy center image
    ilog = dx2 #+ dy2

unsharp image = unitNormalize $ image #+ (image #- gimage)
  where
    gimage = convolve2D maskg center image

gmag image = IM.sqrt $ (dx #* dx) #+ (dy #* dy)
  where
    dx = convolve2D maskdx center image
    dy = convolve2D maskdy center image

gang image = quantizeAngle4 $ IM.atan2 dy $ preventZero dx
  where
    dx = convolve2D maskdx center image
    dy = convolve2D maskdy center image

grad image = (gmag,gang)
  where
    gmag = IM.sqrt $ (dx #* dx) #+ (dy #* dy)
    gang = quantizeAngle4 $ IM.atan2 dy $ preventZero dx
    dx = convolve2D maskdx center image
    dy = convolve2D maskdy center image

ilog image = convolve2D masklog center image

crossesZero (_,ns) = (minimum ns) < 0 && (maximum ns) > 0

crossesZeroMax ((v,ns1),(_,ns2)) =
  (minimum ns2) < 0 && (maximum ns2) > 0 && maxEdge (v,ns1)

maxEdge (v,[]) = False
maxEdge (v,ns) = v >= (maximum ns)

valueToMax ((x,y),_) = ((x,y),1)
valueToColor c ((x,y),_) = ((x,y),c)

zeroCrossings image =
  filterNeighborhood n8 crossesZero image

maxZeroCrossings mag ang log =
  filterNeighborhoodPair ((nes5 ang),ns5) crossesZeroMax (mag,log)

gradientExtrema mag ang =
  filterNeighborhood (nes5 ang) maxEdge mag

usage :: String
usage = "usage: l07-edges [fst|snd|mag|ang|ext|zero|maxzero] source target"

main = do
  (mode,sourceImage,targetImage) <- readArgs
  img <- readFromFile sourceImage
  let
      (mag,ang) = grad img
      tmag = threshold (0,1) 0.05 mag
      mag' = tmag #* mag
      ang' = tmag #* ang
  case mode of
    "unsharp" ->
      saveImage targetImage $ unsharp img
    "fst" ->
      saveImage targetImage $ montage (2,1) 2 $ imageDerivatives1 img
    "snd" ->
      saveImage targetImage $ montage (2,2) 2 $ imageDerivatives2 img
    "mag" ->
      saveImage targetImage mag'
    "ang" ->
      saveImage targetImage $ unitNormalize ang'
    "ext" ->
      saveImage targetImage $ drawPixelsColor (grayToRGB img) $
        map (valueToColor cyan) $ gradientExtrema mag' ang'
    "zero" ->
      saveImage targetImage $ drawPixelsColor (grayToRGB img) $
        map (valueToColor cyan) $ zeroCrossings $ ilog img
    "maxzero" ->
      saveImage targetImage $ drawPixelsColor (grayToRGB img) $
        map (valueToColor cyan) $ maxZeroCrossings mag' ang' (ilog img)
    otherwise -> error usage
