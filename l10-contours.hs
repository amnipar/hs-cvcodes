module Main where

import CV.Image
import CV.Filters
import CV.ImageMathOp
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
import Text.Printf

-- plot width in pixels
width = 400
-- plot height in pixels
height = 300
-- plot margin
margin = 10
-- plot x scale
xscale = 1
-- plot y scale
yscale = 2 * pi
-- y value minimum
ymin = -pi

-- mask used in convolving the signal
-- either averageMask or gaussianMask can be selected, n tells the mask length
mask :: [Float]
mask = gaussianMask1D(maskSize)
--mask = averageMask1D(maskSize)
-- manually defined masks; take care of setting the maskSize correctly
--mask = [0.1,0.2,0.4,0.2,0.1]
--mask = [-2,-1,0,1,2]

-- size of the convolution mask
maskSize :: Int
maskSize = 15

-- index of the mask center
maskCenter :: Int
maskCenter = getMaskCenter1D(maskSize)

toTriples (a:b:c:ps) = (a,b,c):(toTriples (b:c:ps))
toTriples ps = []

contourToAngle :: [(Float,Float)] -> [(Float,Float)]
contourToAngle points = map scale $ zip [0..l-1] angles
  where
    scale (x,a) = ((iToF x) / fl - 0.5, a)
    -- scale (x,a) = ((iToF x) / fl - 0.5, a)
    angles = map pToA $ toTriples ([last points] ++ points ++ [head points])
    pToA ((x1,y1),(x2,y2),(x3,y3)) = atan2 (y3-y1) (x3-x1)
    -- pToA ((x1,y1),(x2,y2),(x3,y3)) = atan2 (iToF $ y3-y1) (iToF $ x3-x1)
    l = length points
    fl = iToF l

contourToDerivative1 :: [(Float,Float)] -> [(Float,Float)]
contourToDerivative1 points =
  map pToD $ toTriples ([last points] ++ points ++ [head points])
  where
    pToD ((x1,y1),(x2,y2),(x3,y3)) = ((x3-x1),(y3-y1))
    --pToD ((x1,y1),(x2,y2),(x3,y3)) = ((iToF $ x3-x1),(iToF $ y3-y1))

derivative1ToDerivative2 :: [(Float,Float)] -> [(Float,Float)]
derivative1ToDerivative2 ds =
  map d1ToD2 $ toTriples ([last ds] ++ ds ++ [head ds])
  where
    d1ToD2 ((dx1,dy1),(dx2,dy2),(dx3,dy3)) = (dx3-dx1,dy3-dy1)

contourToCurvature :: [(Float,Float)] -> [(Float,Float)]
contourToCurvature points =
  map scale $ zip [0..l-1] $ map dToC ds
  where
    l = length points
    fl = iToF l
    scale (x,a) = ((iToF x) / fl - 0.5, a)
    -- scale (x,a) = ((iToF x) / fl - 0.5, a)
    d1s = contourToDerivative1 points
    d2s = derivative1ToDerivative2 d1s
    ds = zip d1s d2s
    dToC ((x1,y1),(x2,y2)) =
      (x1*y2-y1*x2) / (sqrt $ (x1**2 + y1**2)**2)

filterContour :: [(Int,Int)] -> [(Float,Float)]
filterContour points =
  zip
      (map snd $ convolve1D mask maskCenter $ zip (repeat 0) xs)
      (map snd $ convolve1D mask maskCenter $ zip (repeat 0) ys)
  where
    xs = map (iToF.fst) points
    ys = map (iToF.snd) points

roundPair (x,y) = (round x, round y)



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
    c = getContour timg
    fc = filterContour c
    --a = contourToAngle c
    fa = contourToAngle fc
    --d = contourToCurvature c
    fd = contourToCurvature fc
    --pa = signalToPixel (width,height) margin (xscale,yscale) ymin a
    pfa = signalToPixel (width,height) margin (xscale,yscale) ymin fa
    --pd = signalToPixel (width,height) margin (xscale,yscale) ymin d
    pfd = signalToPixel (width,height) margin (xscale,yscale) ymin fd
  --saveImage outputImage $ plotLines cyan 1 c $ convGrayToColor timg
  saveImage outputImage $ montage (1,2) 2
      [ plotLines green 2 (map roundPair fc) $ convGrayToColor timg
      , plotLines red 1 pfd $ -- plotLines dred 1 pfd $
          plotLines blue 1 pfa $ -- plotLines dblue 1 pfa $
          emptyColorImage (width,height) white
      ]
