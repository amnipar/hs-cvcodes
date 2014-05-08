module DrawingUtils
( black,white
, blue,lblue,mblue,dblue,cblue
, green,lgreen,mgreen,dgreen,cgreen
, red,lred,mred,dred,cred
, cyan,lcyan,mcyan,dcyan,ccyan
, magenta,lmagenta,mmagenta,dmagenta,cmagenta
, yellow,lyellow,myellow,dyellow,cyellow
, gray,dgray,lgray,cgray
, xtop
, ptox
, ytop
, ptoy
, signalToPixel
, plotLines
, plotSpikes
, plotPoints
, plotRects
, plotCircles
, plotEllipses
, plotHistogram
, pointsToLines
, pointToRect
, pointsToRects
, pointToCircle
, pointsToCircles
, drawPixelsGray
, drawPixelsColor
, drawFilter
) where

import CV.Image
import CV.Pixelwise
import CV.ImageOp
import CV.Drawing
import Utils.Rectangle hiding (scale)

import Data.Function
import Control.Monad
import System.IO.Unsafe

import BasicUtils

-- Color constants; notice that OpenCV uses BGR colors instead of RGB

black,white :: (Float,Float,Float)

black  = (0,0,0)
white  = (1,1,1)

-- shades of blue
dblue,mblue,lblue,blue :: (Float,Float,Float)
dblue         = (0.25,0.00,0.00)
mblue         = (0.50,0.00,0.00)
lblue         = (0.75,0.00,0.00)
blue          = (1.00,0.00,0.00)
-- a custom blue
cblue :: Float -> (Float,Float,Float)
cblue cval    = (cval,0.00,0.00)

-- shades of green
dgreen,mgreen,lgreen,green :: (Float,Float,Float)
dgreen        = (0.00,0.25,0.00)
mgreen        = (0.00,0.50,0.00)
lgreen        = (0.00,0.75,0.00)
green         = (0.00,1.00,0.00)
-- a custom green
cgreen :: Float -> (Float,Float,Float)
cgreen cval   = (0.00,cval,0.00)

dred,mred,lred,red :: (Float,Float,Float)
dred          = (0.00,0.00,0.25)
mred          = (0.00,0.00,0.50)
lred          = (0.00,0.00,0.75)
red           = (0.00,0.00,1.00)
-- a custom red
cred :: Float -> (Float,Float,Float)
cred cval     = (0.00,0.00,cval)

-- shades of cyan
dcyan,mcyan,lcyan,cyan :: (Float,Float,Float)
dcyan         = (0.25,0.25,0.00)
mcyan         = (0.50,0.50,0.00)
lcyan         = (0.75,0.75,0.00)
cyan          = (1.00,1.00,0.00)
-- a custom cyan
ccyan :: Float -> (Float,Float,Float)
ccyan cval    = (cval,cval,0.00)

-- shades of magenta
dmagenta,mmagenta,lmagenta,magenta :: (Float,Float,Float)
dmagenta      = (0.25,0.00,0.25)
mmagenta      = (0.50,0.00,0.50)
lmagenta      = (0.75,0.00,0.75)
magenta       = (1.00,0.00,1.00)
-- a custom cyan
cmagenta :: Float -> (Float,Float,Float)
cmagenta cval = (cval,0.00,cval)

-- shades of yellow
dyellow,myellow,lyellow,yellow :: (Float,Float,Float)
dyellow       = (0.00,0.25,0.25)
myellow       = (0.00,0.50,0.50)
lyellow       = (0.00,0.75,0.75)
yellow        = (0.00,1.00,1.00)
-- a custom yellow
cyellow :: Float -> (Float,Float,Float)
cyellow cval = (0.00,cval,cval)

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
signalToPixel :: (Int,Int) -> Int -> (Float,Float) -> Float -> [(Float,Float)]
    -> [(Int,Int)]
signalToPixel (w,h) margin (xscale,yscale) miny s =
  map (\(x,y) -> (xtop w margin xscale x, ytop h margin yscale miny y)) s

-- | Draws a list of lines over the image. The lines are given in pixel
--   coordinates.
plotLines :: (Float,Float,Float) -> Int -> [(Int,Int)]
    -> Image RGB D32 -> Image RGB D32
plotLines color size points image =
  image <## [lineOp color size (x1,y1) (x2,y2)
            | ((x1,y1),(x2,y2)) <- pointsToLines points]

-- | Plots a list of points as spikes with a vertical line and a small circle at
--   the top end. The points are given in pixel coordinates.
plotSpikes :: (Float,Float,Float) -> Int -> Int -> Int -> [(Int,Int)]
    -> Image RGB Float -> Image RGB Float
plotSpikes color lineSize pointSize y0 points image =
  image <## [lineOp color lineSize (x,y0) (x,y) | (x,y) <- points]
        <## [circleOp color (x,y) pointSize Filled | (x,y) <- points]

plotPoints :: (Float,Float,Float) -> Int -> [(Int,Int)]
    -> Image RGB Float -> Image RGB Float
plotPoints color size points image = 
  image <## [lineOp color 1 (x-r,y) (x+r,y) | (x,y) <- points]
        <## [lineOp color 1 (x,y-r) (x,y+r) | (x,y) <- points]
  where
    r = size `div` 2

-- | Plots a list of rectangles over the image. The rectangles are given as the
--   top left corner point and (width,height) pair, in pixel coordinates.
plotRects :: (Float,Float,Float) -> Int -> [((Int,Int),(Int,Int))]
    -> Image RGB Float -> Image RGB Float
plotRects color size rects image =
  image <## [rectOp color s (mkRectangle p1 p2) | (p1,p2) <- rects]
  where
    s | size > 0 = size
      | otherwise = (-1)

-- | Plots a list of circles over the image. The circles are given as a point
--   and a radius, in pixel coordinates.
plotCircles :: (Float,Float,Float) -> Int -> [((Int,Int),Int)]
    -> Image RGB Float -> Image RGB Float
plotCircles color size circles image =
  image <## [circleOp color p r s | (p,r) <- circles]
  where
    s | size > 0 = Stroked size
      | otherwise = Filled

-- | Plots a list of ellipses over the image. The ellipses are given as a
--   centerpoint, two radii, and angle. The angle describes the direction in
--   which the longer axis of the ellipse is pointing.
plotEllipses :: (Float,Float,Float) -> Int -> [((Int,Int),(Int,Int),Float)]
    -> Image RGB Float -> Image RGB Float
plotEllipses color size ellipses image =
  image <## [ellipseOp color s p r a (0,360) | (p,r,a) <- ellipses]
  where
    s | size > 0 = size
      | otherwise = (-1)

-- | Plots a histogram onto the given image.
plotHistogram :: (Float,Float,Float) -> Int -> [(Float,Float)]
    -> Image RGB Float -> Image RGB Float
plotHistogram color margin bins image =
  plotRects color 0 (map binToRect (zip bins $ map iToF [0..n-1])) image
  where
    n = length bins
    (w,h) = getSize image
    xscale = iToF n
    xscale' = xscale / 2
    yscale = maximum $ map snd bins
    miny = 0
    binToRect ((_,c),i) = ((x1,y1),(x2-x1,y2-y1))
      where
        x1 = xtop w margin xscale (i - xscale')
        y1 = ytop h margin yscale miny c
        x2 = xtop w margin xscale (i+1 - xscale')
        y2 = ytop h margin yscale miny 0

-- | Converts a list of points to a list of lines; in practice, makes a line
--   from each consecutive pair of points such that lines will form a continuous
--   polyline.
pointsToLines :: [(Int,Int)] -> [((Int,Int),(Int,Int))]
pointsToLines ps = pToL ps []
  where
    pToL [] ls = ls
    pToL (p1:[]) ls = ls
    pToL (p1:p2:ps) ls = [(p1,p2)] ++ (pToL (p2:ps) ls)

pointToRect :: Int -> ((Int,Int),a) -> ((Int,Int),(Int,Int))
pointToRect r ((x,y),_) = ((x-r,y-r),(2*r+1,2*r+1))

-- | Converts a list of points to a list of rects with the specified radius
--   around each point.
pointsToRects :: Int -> [((Int,Int),a)] -> [((Int,Int),(Int,Int))]
pointsToRects s ps = map (pointToRect s) ps

pointToCircle :: Int -> ((Int,Int),a) -> ((Int,Int),Int)
pointToCircle r ((x,y),_) = ((x,y),r)

-- | Converts a list of points to a list of circles with the specified radius
--   around each point.
pointsToCircles :: Int -> [((Int,Int),a)] -> [((Int,Int),Int)]
pointsToCircles r ps = map (pointToCircle r) ps

drawPixelsGray :: Image GrayScale Float -> [((Int,Int),Float)]
    -> Image GrayScale Float
drawPixelsGray img points = unsafePerformIO $ do
  mimg <- toMutable img
  forM_ points $ \(p,v) -> setPixel p v mimg
  fromMutable mimg

drawPixelsColor :: Image RGB Float -> [((Int,Int),(Float,Float,Float))]
    -> Image RGB Float
drawPixelsColor img points = unsafePerformIO $ do
  mimg <- toMutable img
  forM_ points $ \(p,v) -> setPixel p v mimg
  fromMutable mimg

drawFilter :: Int -> ((Int,Int) -> Float) -> Image GrayScale Float
drawFilter s f = imageFromFunction (s,s) f'
  where
    r = s `div` 2
    f' (x,y) = f (x-r,y-r)
