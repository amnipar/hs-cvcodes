module Neighborhoods
( n4
, n8
, ns5
, ne8
, nes5
, create4Neighborhood
, create8Neighborhood
, createS5Neighborhood
, getNeighborhood
, get4Neighborhood
, get8Neighborhood
, getS5Neighborhood
, filterNeighborhood
, filterNeighborhood2
, filterNeighborhoodPair
) where

import CV.Image

import BasicUtils

type Neighborhood = (Float,[Float])
type NeighborhoodFunction = (Int,Int) -> (Int,Int) -> [(Int,Int)]
type ScaleNeighborhood = (Int,Float,[Float])

-- 4-neighborhood: coordinates to the four directly adjacent pixels
--  x
-- xox
--  x
n4 :: (Int,Int) -> (Int,Int) -> [(Int,Int)]
n4 (w,h) (x,y) = nn $ wn $ en $ sn []
  where
    nn ns | y > 0     = (x,y-1):ns
          | otherwise = ns
    wn ns | x > 0     = (x-1,y):ns
          | otherwise = ns
    en ns | x < w-1   = (x+1,y):ns
          | otherwise = ns
    sn ns | y < h-1   = (x,y+1):ns
          | otherwise = ns

-- 8-neighborhood: coordinates to all eight surrounding pixels
-- xxx
-- xox
-- xxx
n8 :: (Int,Int) -> (Int,Int) -> [(Int,Int)]
n8 (w,h) (x,y) = nwn $ nn $ nen $ en $ sen $ sn $ swn $ wn []
  where
    nwn ns | x > 0   && y > 0   = (x-1,y-1):ns
           | otherwise          = ns
    nn  ns |            y > 0   = (x,y-1):ns
           | otherwise          = ns
    nen ns | x < w-1 && y > 0   = (x+1,y-1):ns
           | otherwise          = ns
    en  ns | x < w-1            = (x+1,y):ns
           | otherwise          = ns
    sen ns | x < w-1 && y < h-1 = (x+1,y+1):ns
           | otherwise          = ns
    sn  ns |            y < h-1 = (x,y+1):ns
           | otherwise          = ns
    swn ns | x > 0   && y < h-1 = (x-1,y+1):ns
           | otherwise          = ns
    wn  ns | x > 0              = (x-1,y):ns
           | otherwise          = ns

-- 8-neighborhood in scale space when images are not scaled down; the
-- neighboring pixel in scale 2 is 2 pixels away. The distance to the diagonal
-- neighbor will approach s when s increases.
sn8 :: Int -> (Int,Int) -> (Int,Int) -> [(Int,Int)]
sn8 s (w,h) (x,y) = filter valid $
  [(x-r,y-r),(x,y-s),(x+r,y-r),(x+s,y),(x+r,y+r),(x,y+s),(x-r,y+r),(x-s,y)]
  where
    r = round $ sqrt $ (iToF r)**2 / 2
    valid (x,y) = not $ (x < 0) || (x >= w) || (y < 0) || (y >= h)

-- a spherical neighborhood with diameter 5
--  xxx
-- xxxxx
-- xxoxx
-- xxxxx
--  xxx

ns5 :: (Int,Int) -> (Int,Int) -> [(Int,Int)]
ns5 (w,h) (x,y) = filter valid $
  [(x-1,y-2),(x, y-2),(x+1,y-2),
   (x-2,y-1),(x-1,y-1),(x,y-1),(x+1,y-1),(x+2,y-1),
   (x-2,y),(x-1,y),(x+1,y),(x+2,y),
   (x-2,y+1),(x-1,y+1),(x,y+1),(x+1,y+1),(x+2,y+1),
   (x-1,y+2),(x,y+2),(x+1,y+2)]
  where
    valid (x,y) = not $ (x < 0) || (x >= w) || (y < 0) || (y >= h)

create4Neighborhood (w,h) = n4 (w,h)
create8Neighborhood (w,h) = n8 (w,h)
createS5Neighborhood (w,h) = ns5 (w,h)

-- Edge 8-neighborhood based on quantized gradient direction acquired with
-- quantizeAngle4. Gets the two neighbors across the direction of edge.
ne8 :: Image GrayScale Float -> (Int,Int) -> (Int,Int) -> [(Int,Int)]
ne8 ang (w,h) (x,y)
  | x <= 0 || x >= w-1 || y <= 0 || y >= h-1 = []
  | a == 1 = [(x-1,y),(x+1,y)]
  | a == 2 = [(x-1,y-1),(x+1,y+1)]
  | a == 3 = [(x,y-1),(x,y+1)]
  | a == 4 = [(x-1,y+1),(x+1,y-1)]
  | otherwise = [] -- a could be 0 or something else
  where
    a = getPixel (x,y) ang


-- Edge s5-neighborhood based on quantized gradient direction acquired with
-- quantizeAngle4. Gets the two neighbors across the direction of edge.
nes5 :: Image GrayScale Float -> (Int,Int) -> (Int,Int) -> [(Int,Int)]
nes5 ang (w,h) (x,y)
  | x <= 1 || x >= w-2 || y <= 1 || y >= h-2 = []
  | a == 1 = [(x-2,y),(x-1,y),(x+1,y),(x+2,y)]
  | a == 2 = [(x-2,y-2),(x-1,y-1),(x+1,y+1),(x+2,y+2)]
  | a == 3 = [(x,y-2),(x,y-1),(x,y+1),(x,y+2)]
  | a == 4 = [(x-2,y+2),(x-1,y+1),(x+1,y-1),(x+2,y-2)]
  | otherwise = [] -- a could be 0 or something else
  where
    a = getPixel (x,y) ang

getNeighborhood :: [(Int,Int)] -> Image GrayScale Float -> (Int,Int)
  -> (Float,[Float])
getNeighborhood n i (x,y) = (getPixel (x,y) i, map (p i) n)
  where
    p img (x,y) = getPixel (x,y) img

get4Neighborhood img (x,y) = getNeighborhood (n4 (w,h) (x,y)) img (x,y)
  where
    (w,h) = getSize img

get8Neighborhood img (x,y) = getNeighborhood (n8 (w,h) (x,y)) img (x,y)
  where
    (w,h) = getSize img

getS5Neighborhood img (x,y) = getNeighborhood (ns5 (w,h) (x,y)) img (x,y)
  where
    (w,h) = getSize img

getScaleNeighborhood :: Int -> [Image GrayScale Float] -> (Int,Int)
    -> (ScaleNeighborhood,ScaleNeighborhood,ScaleNeighborhood)
getScaleNeighborhood s images (x,y)
  | s < 2 || (length images) < s+1 = error "scale neighborhood unavailable"
  | otherwise = (n1,n2,n3)
  where
    (w,h) = getSize $ head images
    n1 = (s-1,p1 (x,y),map p1 sn1)
    n2 = (s,p2 (x,y),map p2 sn2)
    n3 = (s+1,p3 (x,y),map p3 sn3)
    p1 (x,y) = getPixel (x,y) (images !! (s-1))
    p2 (x,y) = getPixel (x,y) (images !! s)
    p3 (x,y) = getPixel (x,y) (images !! (s+1))
    sn1 = sn8 (s-1) (w,h) (x,y)
    sn2 = sn8 s (w,h) (x,y)
    sn3 = sn8 (s+1) (w,h) (x,y)

filterNeighborhood :: ((Int,Int) -> (Int,Int) -> [(Int,Int)])
    -> ((Float,[Float]) -> Bool)
    -> Image GrayScale D32 -> [((Int,Int),Float)]
filterNeighborhood neighborhood cond img =
  map (\(p,(v,n)) -> (p,v)) $ filter (cond . snd) $
    [((i,j),(n i j)) | j <- [0..h-1] , i <- [0..w-1]]
  where
    n x y = getNeighborhood (neighborhood (w,h) (x,y)) img (x,y)
    (w,h) = getSize img

filterNeighborhood2 :: ((Int,Int) -> (Int,Int) -> [(Int,Int)])
    -> ((Float,[Float]) -> Bool)
    -> (Image GrayScale D32, Image GrayScale D32) -> [((Int,Int),(Float,Float))]
filterNeighborhood2 neighborhood cond (img1,img2) =
  map (\(p,(v1,_),(v2,_)) -> (p,(v1,v2))) $
    filter (\(_,a,b) -> (cond a) && (cond b)) $
      [ ((i,j),(n1 i j),(n2 i j)) | j <- [0..h-1]  , i <- [0..w-1] ]
  where
    n1 x y = getNeighborhood (neighborhood (w,h) (x,y)) img1 (x,y)
    n2 x y = getNeighborhood (neighborhood (w,h) (x,y)) img2 (x,y)
    (w,h) = getSize img1

filterNeighborhoodPair :: (NeighborhoodFunction, NeighborhoodFunction)
    -> ((Neighborhood,Neighborhood) -> Bool)
    -> (Image GrayScale D32, Image GrayScale D32) -> [((Int,Int),(Float,Float))]
filterNeighborhoodPair (n1,n2) cond (img1,img2) =
  map (\(p,(v1,_),(v2,_)) -> (p,(v1,v2))) $
    filter (\(_,a,b) -> cond (a,b)) $
      [ ((i,j),(pn1 i j),(pn2 i j)) | j <- [0..h-1]  , i <- [0..w-1] ]
  where
    pn1 x y = getNeighborhood (n1 (w,h) (x,y)) img1 (x,y)
    pn2 x y = getNeighborhood (n2 (w,h) (x,y)) img2 (x,y)
    (w,h) = getSize img1
