module Main where

import CV.Image
import CV.Filters

import BasicUtils
import Images
import DrawingUtils

-- plot width in pixels
width = 400
-- plot height in pixels
height = 300
-- plot margin
margin = 10

-- accumulate a histogram.
accHistogram :: Int -> [Float] -> [(Float,Float)]
accHistogram bins xs = norm $ foldr accBin initbins $ xs
  where
    xmin = minimum xs
    xmax = maximum xs
    xrange = xmax - xmin
    xstep = xrange / (iToF bins)
    initbins = [(xmin + (iToF t) * xstep, 0) | t <- [1..bins]]
    toIdx x = floor $ (x - xmin) / xstep
    accBin x bs = findBin x bs
    findBin x ((l,c):[]) = (l,c+1):[]
    findBin x ((l,c):bs)
      | x < l = (l,c+1):bs
      | otherwise = (l,c):(findBin x bs)
    norm bs = map (\(l,c) -> (l,c/s)) bs
      where s = sum $ map snd bs

imgToVals :: Image GrayScale Float -> [Float]
imgToVals img = [getPixel (x,y) img | x <- [0..w-1], y <- [0..h-1]]
  where (w,h) = getSize img

plotHistogram :: (Float,Float,Float) -> (Int,Int) -> Int -> [(Float,Float)]
    -> Image RGB Float -> Image RGB Float
plotHistogram color (w,h) margin bins image =
  plotRects color 0 (map binToRect (zip bins $ map iToF [0..n-1])) image
  where
        n = length bins
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

main = do
  img <- readFromFile "boat.jpg"
  let
      vals = imgToVals $ gaussian (3,3) img
      bins = accHistogram 100 vals
  saveImage "hist.png" $
    plotHistogram black (width,height) margin bins $
      emptyColorImage (width,height) white
