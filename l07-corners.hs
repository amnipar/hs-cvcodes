module Main where

import CV.Image
import CV.Filters
import CV.ImageMath as IM hiding (not)
import CV.ImageMathOp
import CV.ColourUtils
import CV.Pixelwise
import CV.Drawing
import CV.ImageOp
import CV.Operations

import Filters
import Gaussian

import ReadArgs
import Control.Monad
 
-- coordinates to the four directly adjacent pixels
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

-- coordinates to all eight surrounding pixels
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

getNeighborhood :: [(Int,Int)] -> Image GrayScale Float -> (Int,Int)
  -> (Float,[Float])
getNeighborhood n i (x,y) = (getPixel (x,y) i, map (p i) n)
  where
    p img (x,y) = getPixel (x,y) img

get4Neighborhood img (w,h) (x,y) = getNeighborhood (n4 (w,h) (x,y)) img (x,y)
get8Neighborhood img (w,h) (x,y) = getNeighborhood (n8 (w,h) (x,y)) img (x,y)
getS5Neighborhood img (w,h) (x,y) = getNeighborhood (ns5 (w,h) (x,y)) img (x,y)

filterNeighborhood :: ((Float,[Float]) -> Bool) -> Image GrayScale D32 -> [((Int,Int),Float)]
filterNeighborhood cond img = map (\(p,(v,n)) -> (p,v)) $ filter (cond . snd) $
    [ ((i,j),(n i j)) | j <- [0..h-1]  , i <- [0..w-1] ]
  where
    n x y = getS5Neighborhood img (w,h) (x,y)
    (w,h) = getSize img

filterNeighborhood2 :: ((Float,[Float]) -> Bool) -> 
    (Image GrayScale D32, Image GrayScale D32) -> [((Int,Int),Float,Float)]
filterNeighborhood2 cond (img1,img2) = 
  map (\(p,(v1,_),(v2,_)) -> (p,v1,v2)) $
    filter (\(_,a,b) -> (cond a) && (cond b)) $
      [ ((i,j),(n1 i j),(n2 i j)) | j <- [0..h-1]  , i <- [0..w-1] ]
  where
    n1 x y = getS5Neighborhood img1 (w,h) (x,y)
    n2 x y = getS5Neighborhood img2 (w,h) (x,y)
    (w,h) = getSize img1

filterPixels :: (Float -> Bool) -> Image GrayScale D32 -> [((Int,Int),Float)]
filterPixels cond img = filter (cond . snd) $
    [ ((i,j),(v i j)) | j<-[0..height-1] , i <- [0..width-1] ]
  where
    v x y = getPixel (x,y) img
    (width,height) = getSize img

-- filter out corners that are weaker than factor * maximum corner value
relativeThresholdCorners :: Float -> [((Int,Int),Float)] -> [((Int,Int),Float)]
relativeThresholdCorners factor cs = filter ((>t) . snd) cs
  where
    m = maximum $ map snd cs
    t = factor * m

-- filter out corners that are weaker than factor * maximum corner value
relativeThresholdCorners2 :: Float -> [((Int,Int),Float,Float)]
    -> [((Int,Int),Float,Float)]
relativeThresholdCorners2 factor cs = 
  filter (\(_,v1,v2) -> (v1 > t1) && (v2 > t2)) cs
  where
    v1 (_,v,_) = v
    v2 (_,_,v) = v
    m1 = maximum $ map v1 cs
    m2 = maximum $ map v2 cs
    t1 = factor * m1
    t2 = factor * m2

isMaximal (v,ns) = v > (maximum ns)

--      | a   b | a11 a12
-- A =  |       |
--      | c   d | a21 a22

-- T=a+d D=ad-bc

-- L1 = T/2 + (T2/4-D)1/2
-- L2 = T/2 - (T2/4-D)1/2

harrisResponse :: Image GrayScale D32 -> Image GrayScale D32
harrisResponse image = stretchHistogram $ det #- (kappa |* (tra #* tra))
  where
    det = (a11 #* a22) #- (a12 #* a21)
    tra = a11 #+ a22
    dx = convolve2D maskdx center image
    dy = convolve2D maskdy center image
    a11 = gaussian (5,5) $ dx #* dx
    a22 = gaussian (5,5) $ dy #* dy
    a12 = gaussian (5,5) $ dx #* dy
    a21 = a12
    kappa = 0.15

harrisLambda :: Image GrayScale D32 -> Image GrayScale D32
harrisLambda image = stretchHistogram $ IM.min lambda1 lambda2
  where
    det = (a11 #* a22) #- (a12 #* a21)
    tra = a11 #+ a22
    htra = 0.5 |* tra
    stra24 = IM.sqrt $ (0.25 |* (tra #* tra)) #- det
    lambda1 = htra #+ stra24
    lambda2 = htra #- stra24
    dx = convolve2D maskdx center image
    dy = convolve2D maskdy center image
    a11 = gaussian (5,5) $ dx #* dx
    a22 = gaussian (5,5) $ dy #* dy
    a12 = gaussian (5,5) $ dx #* dy
    a21 = a12

hessian :: Image GrayScale D32 -> (Image GrayScale D32,Image GrayScale D32)
hessian image = (idoh,ilog)
  where
    dx2 = convolve2D maskdx2 center image
    dy2 = convolve2D maskdy2 center image
    dxdy = convolve2D maskdxdy center image
    idoh = stretchHistogram $ (dx2 #* dy2) #- (dxdy #* dxdy)
    ilog = stretchHistogram $ IM.sqrt $ (dx2 #+ dy2) #* (dx2 #+ dy2)

maximalHarris :: Image GrayScale D32 -> Image RGB D32
maximalHarris kuva = 
  grayToRGB kuva
    <## [circleOp (0,0,1) (x,y) 5 Filled | ((x,y),v) <- kulmat]
  where
    kulmat = relativeThresholdCorners 0.1 $ filterNeighborhood isMaximal $
        harrisLambda kuva

maximalHessian :: Image GrayScale D32 -> Image RGB D32
maximalHessian kuva =
  grayToRGB kuva
    <## [circleOp (0,0,1) (x,y) 5 Filled | ((x,y),_,_) <- kulmat]
  where
    kulmat = relativeThresholdCorners2 0.4 $ filterNeighborhood2 isMaximal $
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
    dx = convolve2D maskdx center kuva
    dy = convolve2D maskdy center kuva
    a11 = gaussian (5,5) $ dx #* dx
    a22 = gaussian (5,5) $ dy #* dy
    a12 = gaussian (5,5) $ dx #* dy
    a21 = gaussian (5,5) $ dy #* dx
    kappa = 0.10
    r = ((a11 #* a22) #- (a12 #* a21)) #- (kappa |* ((a11 #+ a22) #* (a11 #+ a22)))

imageFile = "nut.png"

sigma = 1
-- mask should fit 6 sigma
size = 7
center = getMaskCenter2D size
maskdx = createMask2D (gaussian2Ddx sigma) size
maskdy = createMask2D (gaussian2Ddy sigma) size
maskdx2 = createMask2D (gaussian2Ddx2 sigma) size
maskdy2 = createMask2D (gaussian2Ddy2 sigma) size
maskdxdy = createMask2D (gaussian2Ddxdy sigma) size

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
    "harris" ->
      saveImage targetImage $ maximalHarris img
    "hessian" ->
      saveImage targetImage $ maximalHessian img
    "rhessian" ->
      saveImage targetImage $ montage (2,1) 2 $ pairToList $ hessian img
    otherwise -> error usage
