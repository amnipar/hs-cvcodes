module Main where

import CV.Image
import CV.ImageMath as IM
import CV.ImageMathOp
import CV.Filters
import CV.Pixelwise
import CV.ColourUtils
import CV.Operations

import Filters
import Gaussian
import Thresholding
import Neighborhoods

import ReadArgs
import Control.Monad
import System.IO.Unsafe

sigma = 1
-- mask should fit 6 sigma
size = 7
center = getMaskCenter2D size
maskdx = createMask2D (gaussian2Ddx sigma) size
maskdy = createMask2D (gaussian2Ddy sigma) size
maskdx2 = createMask2D (gaussian2Ddx2 sigma) size
maskdy2 = createMask2D (gaussian2Ddy2 sigma) size
maskdxdy = createMask2D (gaussian2Ddxdy sigma) size

preventZero image = mapImage c image
  where
        c v | v > 0.0001 = v
            | v < 0.0001 = v
            | v >= 0     = 0.0001
            | otherwise  = -0.0001

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

gmag image = stretchHistogram $ IM.sqrt $ (dx #* dx) #+ (dy #* dy)
  where
    dx = convolve2D maskdx center image
    dy = convolve2D maskdy center image

gang image = unitNormalize $ quantizeAngle4 $ IM.atan2 dy $ preventZero dx
  where
    dx = convolve2D maskdx center image
    dy = convolve2D maskdy center image

ilog image = dx2 #+ dy2
  where
    dx2 = convolve2D maskdx2 center image
    dy2 = convolve2D maskdy2 center image

crossesZero (_,ns) = (minimum ns) < 0 && (maximum ns) > 0

crossesZeroMax ((v,ns1),(_,ns2)) =
  (minimum ns2) < 0 && (maximum ns2) > 0 && v >= (maximum ns1)

valueToMax ((x,y),_) = ((x,y),1)
valueToMax2 ((x,y),(_,_)) = ((x,y),1)

zeroCrossings image = drawPixels (w,h) $
  map valueToMax $ filterNeighborhood n8 crossesZero image
  where
    (w,h) = getSize image

maxZeroCrossings gmag ilog = drawPixels (w,h) $
  map valueToMax2 $ filterNeighborhoodPair ns5 crossesZeroMax $ (gmag,ilog)
  where
    (w,h) = getSize gmag

drawPixels :: (Int,Int) -> [((Int,Int),Float)] -> Image GrayScale Float
drawPixels size points = unsafePerformIO $ do
  mimg <- toMutable $ empty size
  forM_ points $ \(p,v) -> setPixel p v mimg
  fromMutable mimg

usage :: String
usage = "usage: l07-edges [fst|snd|mag|ang|zero|maxzero] source target"

main = do
  (mode,sourceImage,targetImage) <- readArgs
  img <- readFromFile sourceImage
  case mode of
    "fst" ->
      saveImage targetImage $ montage (1,2) 2 $ imageDerivatives1 img
    "snd" ->
      saveImage targetImage $ montage (2,2) 2 $ imageDerivatives2 img
    "mag" ->
      saveImage targetImage $ gmag img
    "ang" ->
      saveImage targetImage $ gang img
    "zero" ->
      saveImage targetImage $ zeroCrossings $ ilog img
    "maxzero" ->
      saveImage targetImage $ maxZeroCrossings (gmag img) (ilog img)
    otherwise -> error usage
