module DrawingUtils
( xtop
, ptox
, ytop
, ptoy
, toPoints
, toLines
, plotLines
, plotSpikes
, emptyGrayImage
, emptyColorImage
) where

import CV.Image
import CV.Pixelwise
import CV.ImageOp
import CV.Drawing

import Data.Function

fi = fromIntegral

xtop :: Int -> Int -> Float -> Float -> Int
xtop width margin scale x =
  margin + (floor $ ((x + (scale / 2)) / scale) * swidth)
  where swidth = (fi $ width - 2 * margin)

ptox :: Int -> Int -> Float -> Int -> Float
ptox width margin scale p =
  scale * ((fi $ p - margin) - (swidth / 2)) / swidth
  where swidth = (fi $ width - 2 * margin)

ytop :: Int -> Int -> Float -> Float -> Float -> Int
ytop height margin scale miny y =
  (floor $ -((y - miny) / scale) * sheight + sheight) + margin
  where sheight = (fi $ height - 2 * margin)

ptoy :: Int -> Int -> Float -> Float -> Int -> Float
ptoy height margin scale miny p =
  -((fi $ p - margin) - sheight) / sheight + miny
  where sheight = (fi $ height - 2 * margin)

pfrags :: Int -> Int -> Int -> Float -> Int -> [Float]
pfrags n width margin scale p =
  [px + scale * (fi i / fi n) / swidth | i <- [-k..k]]
  where
        px = ptox width margin scale p
        swidth = (fi $ width - 2 * margin)
        k | odd n     =  n    `div` 2
          | otherwise = (n-1) `div` 2

toPoints :: (Int,Int) -> Int -> (Float,Float) -> Float -> [(Float,Float)]
    -> [(Int,Int)]
toPoints (w,h) margin (xscale,yscale) miny s =
  map (\(x,y) -> (xtop w margin xscale x, ytop h margin yscale miny y)) s

toLines :: [(Int,Int)] -> [((Int,Int),(Int,Int))] -> [((Int,Int),(Int,Int))]
toLines [] ls = ls
toLines (p1:[]) ls = ls
toLines (p1:p2:ps) ls = [(p1,p2)] ++ (toLines (p2:ps) ls)

plotLines :: (D32,D32,D32) -> Int -> [(Int,Int)] 
    -> Image RGB D32 -> Image RGB D32
plotLines color size points image =
  image <## [lineOp color size (x1,y1) (x2,y2) 
            | ((x1,y1),(x2,y2)) <- toLines points []]

plotSpikes :: (D32,D32,D32) -> Int -> Int -> Int -> [(Int,Int)]
    -> Image RGB D32 -> Image RGB D32
plotSpikes color lineSize pointSize y0 points image =
  image <## [lineOp color lineSize (x,y0) (x,y) | (x,y) <- points]
        <## [circleOp color (x,y) pointSize Filled | (x,y) <- points]

plot :: (Int,Int) -> Int -> Float -> Float -> (Float -> Float)
    -> Image GrayScale D32
plot (w,h) margin xscale yscale f = imageFromFunction (w,h) g
  where
    domain = [ptox w margin xscale p | p <- [margin..w-margin-1]]
    miny = min 0 $ minimum $ map f domain
    maxy = maximum $ map f domain
    --yscale = maxy - miny
    g (x,y)
      | x < margin || x > w-margin || y < margin || y > h-margin = 0
      | not $ null yvals = 1
      | otherwise        = 0
        where
          xvals = pfrags 10 w margin xscale x
          yvals = filter (==y) $ map ((ytop h margin yscale miny).f) xvals

plotSampled :: (Int,Int) -> Int -> Float -> Float -> [(Float,Float)]
    -> Image GrayScale D32
plotSampled (w,h) margin xscale yscale f = imageFromFunction (w,h) g
  where
    miny = min 0 $ minimum $ map snd f
    maxy = maximum $ map snd f
    pf = map (\(x,fx) -> (xtop w margin xscale x, ytop h margin yscale miny fx)) f
    --yscale = maxy - miny
    g (x,y)
      | x < margin || x > w-margin || y < margin || y > h-margin = 0
      | (not $ null xvals) && y >= yval = 1
      | otherwise = 0
        where
          xvals = filter ((==x).fst) pf
          yval = snd $ head xvals


emptyGrayImage :: (Int,Int) -> D32 -> Image GrayScale D32
emptyGrayImage (w,h) color = imageFromFunction (w,h) (const color)

emptyColorImage :: (Int,Int) -> (D32,D32,D32) -> Image RGB D32
emptyColorImage (w,h) color = imageFromFunction (w,h) (const color)
