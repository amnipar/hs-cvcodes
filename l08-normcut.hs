{-#LANGUAGE ParallelListComp#-}
-- this program requires lots of memory and may take minutes to run.
-- small images should be preferred; 100x100 image has 10,000 pixels and the
-- generated laplacian matrix will have 100,000,000 entries; filled with double
-- precision numbers, it takes 800M of memory.
-- to improve memory usage, you can try compiling the program with
-- ghc --make -rtsopts -threaded -O2 l08-normcut.hs
-- and running it with
-- l08-normcut +RTS -N2 -C0 -c -H3G -m2 -RTS
-- if your computer has more cores than 2, -N2 can be bigger
-- if your computer has more memory, -H3G can be larger
module Main where

import CV.Image
import CV.ColourUtils
import CV.Filters

import Images
import Thresholding

import System.IO.Unsafe
import Data.List
import Data.Ord

import qualified Data.Packed.Vector as V
import qualified Data.Packed.Matrix as M
import Numeric.LinearAlgebra as LA

-- index pixel 'nodes' as row*w+col.
-- for each 'node', create neighborhood as offsets and convert these to indices.
-- generate a sparse laplacian matrix row for each node.

-- the four pixels directly adjacent
n4 :: (Int,Int) -> (Int,Int) -> [(Int,Int,Float)]
n4 (w,h) (x,y) = nn $ wn $ en $ sn []
  where
    nn ns | y > 0     = (x,y-1,1):ns
          | otherwise = ns
    wn ns | x > 0     = (x-1,y,1):ns
          | otherwise = ns
    en ns | x < w-1   = (x+1,y,1):ns
          | otherwise = ns
    sn ns | y < h-1   = (x,y+1,1):ns
          | otherwise = ns

-- coordinates to all eight surrounding pixels
n8 :: (Int,Int) -> (Int,Int) -> [(Int,Int,Float)]
n8 (w,h) (x,y) = nwn $ nn $ nen $ en $ sen $ sn $ swn $ wn []
  where
    isqrt2 = 1 / (sqrt 2)
    nwn ns | x > 0   && y > 0   = (x-1,y-1,isqrt2):ns
           | otherwise          = ns
    nn  ns |            y > 0   = (x,y-1,1):ns
           | otherwise          = ns
    nen ns | x < w-1 && y > 0   = (x+1,y-1,isqrt2):ns
           | otherwise          = ns
    en  ns | x < w-1            = (x+1,y,1):ns
           | otherwise          = ns
    sen ns | x < w-1 && y < h-1 = (x+1,y+1,isqrt2):ns
           | otherwise          = ns
    sn  ns |            y < h-1 = (x,y+1,1):ns
           | otherwise          = ns
    swn ns | x > 0   && y < h-1 = (x-1,y+1,isqrt2):ns
           | otherwise          = ns
    wn  ns | x > 0              = (x-1,y,1):ns
           | otherwise          = ns

pixelToIndex :: (Int,Int) -> ((Int,Int),Float) -> (Int,Float)
pixelToIndex (w,h) ((x,y),v) = (y*w+x,v)

toLWeight :: Float -> (Int,Float) -> (Int,Float)
toLWeight v1 (i,v2) = (i,-((1 - (min 1 $ sqrt $ (4*v1-4*v2)**2))/4))

-- a neighboring node will have weight -w/8
graphNeighborhood :: ((Int,Int) -> [(Int,Int,Float)]) -> Image GrayScale Float
  -> (Int,Int) -> (((Int,Int),Float),[((Int,Int),Float)])
graphNeighborhood n img (x,y) = (((x,y),v1),map (p img) (n (x,y)))
  where
    v1 = getPixel (x,y) img
    p img (nx,ny,nw) = ((nx,ny),w)
      where
        w = -((nw * (1 - (abs $ v1-v2)))/8)
        v2 = getPixel (nx,ny) img

graphLaplacianRow :: (Int,Int) -> Image GrayScale Float -> (Int,Int)
    -> Vector Double
graphLaplacianRow (w,h) img (x,y) = V.fromList $ fst $
    foldl' accL ([],n') [s-1,s-2..0]
  where
    s = w*h
    y' = y*w+x
    ((_,v),n) = graphNeighborhood (n8 (w,h)) img (x,y)
    -- each node itself will have weight 1
    n' = reverse $ sortBy (comparing fst) $
        ((y',1):(map (pixelToIndex (w,h)) n))
    accL (ls,[]) i                     = (             0:ls,        [])
    accL (ls,(i1,v):is) i2 | i1 == i2  = ((realToFrac v):ls,        is)
                           | otherwise = (             0:ls, (i1,v):is)

-- create a graph laplacian matrix for an image by generating one row of
-- neighborhood distances per each image pixel
graphLaplacian :: Image GrayScale Float -> Matrix Double
graphLaplacian img = M.fromRows $ map (graphLaplacianRow (w,h) img) ps
  where
    (w,h) = getSize img
    ps = [(x,y) | y <- [0..h-1] , x <- [0..w-1]]

renderVector :: (Int,Int) -> V.Vector Double -> Image GrayScale Float
renderVector (w,h) vec = unsafePerformIO $ do
  img <- toMutable $ empty (w,h)
  sequence_ [(setPixel (x,y) (realToFrac e) img)
             | e <- V.toList vec
             | (x,y) <- cross [0..w-1] [0..h-1]]
  fromMutable(img)

cross xs ys = [(x,y) | y <- ys , x <- xs]

main = do
  img <- readFromFile "rect2-tiny.png"
  let
    --img = discGrayImage 20 5 1 0
    (w,h) = getSize img
    l = graphLaplacian img
    (val,vec) = eigSH l
    vlist = toList val
    s = length vlist
    -- get the eigenvectors corresponding to smallest eigenvalues (except last)
    evec = drop (s-8-1) $ init $ toColumns vec
  saveImage "cuts.png" $ montage (4,2) 2 $
      map (stretchHistogram.(renderVector (w,h))) evec
  --saveImage "cuts.png" $ montage (4,2) 2 $
  --    map ((threshold (1,0) 0).(renderVector (w,h))) evec
