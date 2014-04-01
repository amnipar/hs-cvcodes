{-# LANGUAGE FlexibleContexts #-}
module DrawingUtils
( black,white
, blue,lblue,mblue,dblue,cblue
, green,lgreen,mgreen,dgreen,cgreen
, red,lred,mred,dred,cred
, gray,dgray,lgray,cgray
, xtop
, ptox
, ytop
, ptoy
, toPoints
, toLines
, plotLines
, plotSpikes
, plotRects
, naiveUpscale
, emptyGrayImage
, emptyColorImage
, resizeImage
, resizeImageFaithful
, scaleImage
) where

import CV.Image
import CV.Pixelwise
import CV.ImageOp
import CV.Drawing
import CV.Transforms
import Utils.Rectangle hiding (scale)

import Data.Function

import BasicUtils

-- Color constants; notice that OpenCV uses BGR colors instead of RGB

black,white :: (Float,Float,Float)

black  = (0,0,0)
white  = (1,1,1)

-- shades of blue
dblue,mblue,lblue,blue :: (Float,Float,Float)
dblue       = (0.25,0.00,0.00)
mblue       = (0.50,0.00,0.00)
lblue       = (0.75,0.00,0.00)
blue        = (1.00,0.00,0.00)
-- a custom blue
cblue :: Float -> (Float,Float,Float)
cblue cval  = (cval,0.00,0.00)

-- shades of green
dgreen,mgreen,lgreen,green :: (Float,Float,Float)
dgreen      = (0.00,0.25,0.00)
mgreen      = (0.00,0.50,0.00)
lgreen      = (0.00,0.75,0.00)
green       = (0.00,1.00,0.00)
-- a custom green
cgreen :: Float -> (Float,Float,Float)
cgreen cval = (0.00,cval,0.00)

dred,mred,lred,red :: (Float,Float,Float)
dred        = (0.00,0.00,0.25)
mred        = (0.00,0.00,0.50)
lred        = (0.00,0.00,0.75)
red         = (0.00,0.00,1.00)
-- a custom red
cred :: Float -> (Float,Float,Float)
cred cval   = (0.00,0.00,cval)

-- shades of gray
dgray,lgray,gray :: (Float,Float,Float)
dgray       = (0.25,0.25,0.25)
gray        = (0.50,0.50,0.50)
lgray       = (0.75,0.75,0.75)
-- a custom gray
cgray :: Float -> (Float,Float,Float)
cgray cval  = (cval,cval,cval)

-- | Converts x values to pixel coordinates, given a plotting context
xtop :: Int -> Int -> Float -> Float -> Int
xtop width margin scale x =
  margin + (floor $ ((x + (scale / 2)) / scale) * swidth)
  where swidth = (iToF $ width - 2 * margin)

-- | Converts pixel coordinates to x values, given a plotting context
ptox :: Int -> Int -> Float -> Int -> Float
ptox width margin scale p =
  scale * ((iToF $ p - margin) - (swidth / 2)) / swidth
  where swidth = (iToF $ width - 2 * margin)

-- | Converts y values to pixel coordinates, given a plotting context
ytop :: Int -> Int -> Float -> Float -> Float -> Int
ytop height margin scale miny y =
  (floor $ -((y - miny) / scale) * sheight + sheight) + margin
  where sheight = (iToF $ height - 2 * margin)

-- | Converts pixel coordinates to y values, given a plotting context
ptoy :: Int -> Int -> Float -> Float -> Int -> Float
ptoy height margin scale miny p =
  -((iToF $ p - margin) - sheight) / sheight + miny
  where sheight = (iToF $ height - 2 * margin)

-- | Converts a list of (x,y) pairs to points in pixel coordinates, given a
--   plotting context (which involves the plot dimensions, margins, scales of
--   coordinate axes and y axis min value; x axis is always centered at the
--   origin).
toPoints :: (Int,Int) -> Int -> (Float,Float) -> Float -> [(Float,Float)]
    -> [(Int,Int)]
toPoints (w,h) margin (xscale,yscale) miny s =
  map (\(x,y) -> (xtop w margin xscale x, ytop h margin yscale miny y)) s

-- | Converts a list of points to a list of lines; in practice, makes a line
--   from each consecutive pair of points such that lines will form a continuous
--   polyline.
toLines :: [(Int,Int)] -> [((Int,Int),(Int,Int))] -> [((Int,Int),(Int,Int))]
toLines [] ls = ls
toLines (p1:[]) ls = ls
toLines (p1:p2:ps) ls = [(p1,p2)] ++ (toLines (p2:ps) ls)

-- | Draws a list of lines over the image. The lines are given in pixel
--   coordinates.
plotLines :: (Float,Float,Float) -> Int -> [(Int,Int)]
    -> Image RGB D32 -> Image RGB D32
plotLines color size points image =
  image <## [lineOp color size (x1,y1) (x2,y2)
            | ((x1,y1),(x2,y2)) <- toLines points []]

-- | Plots a list of points as spikes with a vertical line and a small circle at
--   the top end. The points are given in pixel coordinates.
plotSpikes :: (Float,Float,Float) -> Int -> Int -> Int -> [(Int,Int)]
    -> Image RGB Float -> Image RGB Float
plotSpikes color lineSize pointSize y0 points image =
  image <## [lineOp color lineSize (x,y0) (x,y) | (x,y) <- points]
        <## [circleOp color (x,y) pointSize Filled | (x,y) <- points]

plotPoints :: (Float,Float,Float) -> Int -> [(Int,Int)]
    -> Image RGB Float -> Image RGB Float
plotPoints color size points image = image

plotRects :: (Float,Float,Float) -> Int -> [((Int,Int),(Int,Int))]
    -> Image RGB Float -> Image RGB Float
plotRects color size rects image =
  image <## [rectOp color s (mkRectangle p1 p2) | (p1,p2) <- rects]
  where
        s | size > 0 = size
          | otherwise = (-1)

plotCircles :: (Float,Float,Float) -> Int -> [((Int,Int),Int)]
    -> Image RGB Float -> Image RGB Float
plotCircles color size cirles image = image

-- | Creates a naively upscaled version of the image by replicating the pixels
--   s times in both directions.
naiveUpscale :: Int -> Image GrayScale D32 -> Image GrayScale D32
naiveUpscale s image = imageFromFunction (nw,nh) f
  where
    (w,h) = getSize image
    nw = s*w
    nh = s*h
    f (x,y) = getPixel (x',y') image
      where
        x' = x `div` s
        y' = y `div` s

-- | Creates an empty grayscale image of the specific shade. Can be used as a
--   starting point for drawing operations.
emptyGrayImage :: (Int,Int) -> D32 -> Image GrayScale Float
emptyGrayImage (w,h) color = imageFromFunction (w,h) (const color)

-- | Creates an empty color image of the specific shade. Can be used as a
--   starting point for drawing operations.
emptyColorImage :: (Int,Int) -> (Float,Float,Float) -> Image RGB Float
emptyColorImage (w,h) color = imageFromFunction (w,h) (const color)

-- | Forces the image to given size, not considering the aspect ratio.
resizeImage :: (CreateImage (Image c Float)) =>
  (Int,Int) -> Image c Float -> Image c Float
resizeImage size image = scaleToSize Cubic False size image

-- | Resize the image to given size, preserving the aspect ratio.
resizeImageFaithful :: (CreateImage (Image c Float)) =>
  (Int,Int) -> Image c Float -> Image c Float
resizeImageFaithful size image = scaleToSize Cubic True size image

scaleImage :: (CreateImage (Image c Float)) =>
  (Float,Float) -> Image c Float -> Image c Float
scaleImage ratio image = scale Cubic ratio image
