{-#LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module Main where

import CV.Image
import CV.Filters
import CV.ImageOp
import CV.ImageMathOp
import CV.Drawing
import CV.Operations

import BasicUtils
import DrawingUtils
import Images
import Filters
import Histogram
import Thresholding
import Neighborhoods
import Shapes
import Random

import ReadArgs
import Debug.Trace
import Control.Monad
import Control.Applicative hiding (empty)
import System.Directory
import System.FilePath
import qualified Data.ByteString as B
import Data.Serialize as S
import qualified Data.Packed.Vector as V
import qualified Data.Packed.Matrix as M
import Numeric.LinearAlgebra as LA
import Data.List as L
import Data.Ord
import Data.Trees.KdTree as KD

type RawMoments = (Double,Double,Double,Double,Double,Double,Double,Double,Double,Double)
type CentralMoments = (Double,Double,Double,Double,Double,Double,Double,Double,Double,Double)
type HuMoments = (Double,Double,Double,Double,Double,Double,Double,Double)

objectPixel :: ((Int,Int),Float) -> Bool
objectPixel (_,v) = v == 1

rawMoments :: Image GrayScale Float -> RawMoments
rawMoments image = mom
  where
    op = filter objectPixel $ getPixels image
    mom = foldr accMom (0,0,0,0,0,0,0,0,0,0) op
    accMom ((x,y),v) (m00,m10,m01,m11,m20,m02,m30,m21,m12,m03) =
      (m00',m10',m01',m11',m20',m02',m30',m21',m12',m03')
      where
        fx = fromIntegral x
        fy = fromIntegral y
        fv = realToFrac v
        m00' = m00 + fv
        m10' = m10 + fx * fv
        m01' = m01 + fy * fv
        m11' = m11 + fx * fy * fv
        m20' = m20 + fx**2 * fv
        m02' = m02 + fy**2 * fv
        m30' = m30 + fx**3 * fv
        m21' = m21 + fx**2 * fy * fv
        m12' = m12 + fx * fy**2 * fv
        m03' = m03 + fy**3 * fv

centralMoments :: RawMoments -> CentralMoments
centralMoments (m00,m10,m01,m11,m20,m02,m30,m21,m12,m03) =
  (m00,cx,cy,c11,c20,c02,c30,c21,c12,c03)
  where
    cx = m10/m00
    cy = m01/m00
    c11 = m11 - cx*m01
    c20 = m20 - cx*m10
    c02 = m02 - cy*m01
    c30 = m30 - 3 * cx * m20 + 2 * cx**2 * m10
    c21 = m21 - 2 * cx * m11 - cy * m20 + 2 * cx**2 * m01
    c12 = m12 - 2 * cy * m11 - cx * m02 + 2 * cy**2 * m10
    c03 = m03 - 3 * cy * m02 + 2 * cy**2 * m01

huMoments :: CentralMoments -> HuMoments
huMoments (c00,cx,cy,c11,c20,c02,c30,c21,c12,c03) =
  (hu1,hu2,hu3,hu4,hu5,hu6,hu7,hu8)
  where
    --e10 = c10 / (c00**1.5)
    --e01 = c01 / (c00**1.5)
    e20 = c20 / (c00**2)
    e11 = c11 / (c00**2)
    e02 = c02 / (c00**2)
    e30 = c30 / (c00**2.5)
    e21 = c21 / (c00**2.5)
    e12 = c12 / (c00**2.5)
    e03 = c03 / (c00**2.5)
    hu1 = e20 + e02
    hu2 = (e20 - e02)**2 + 4 * e11**2
    hu3 = (e30 - 3 * e12)**2 + (3 * e21 - e03)**2
    hu4 = (e30 + e12)**2 + (e21 + e03)**2
    hu5 = (e30 - 3*e12) * (e30 + e12)*((e30 + e12)**2 - 3*(e21 + e03)**2) +
        (3*e21 - e03) * (e21 + e03) * (3*(e30 + e12)**2 - (e21 + e03)**2)
    hu6 = (e20 - e02) * ((e30 + e12)**2 - (e21 + e03)**2) +
        4*e11 * (e30 + e12) * (e21 + e03)
    hu7 = (3*e21 - e03) * (e30 + e12) * ((e30 + e12)**2 - 3*(e21 + e03)**2) -
        (e30 - 3*e12) * (e21 + e03) * (3*(e30 + e12)**2 - (e21 + e03)**2)
    hu8 = e11 * ((e30 + e12)**2 - (e03 + e21)**2) -
        (e20 - e02) * (e30 + e12) * (e03 + e21)

gaussianRandomBasis :: (HuMoments,HuMoments)
gaussianRandomBasis = traceShow (dot basis) $ basis
  where
    basis = head $ L.sortBy (comparing dot) $ map (\i -> b) [1..100]
    b = (b1,b2)
      where
        g = map realToFrac $ getGaussianVector (1/16) 16
        [b11,b12,b13,b14,b15,b16,b17,b18] = take 8 g
        [b21,b22,b23,b24,b25,b26,b27,b28] = drop 8 g
        b1 = (b11,b12,b13,b14,b15,b16,b17,b18)
        b2 = (b21,b22,b23,b24,b25,b26,b27,b28)
    dot ((b11,b12,b13,b14,b15,b16,b17,b18),(b21,b22,b23,b24,b25,b26,b27,b28)) =
      b11*b21 + b12*b22 + b13*b23 + b14*b24 + b15*b25 + b16*b26 + b17*b27 + b18*b28

sparseRandomBasis :: (HuMoments,HuMoments)
sparseRandomBasis = traceShow d $ (b1,b2)
  where
    n = 8
    density = 1 / sqrt n
    s = 1 / density
    l1 = 1 / (2*s)
    l2 = l1 + (1 / (2*s))
    v = map (realToFrac.uToV) $ getUniformVector 0 1 16
    uToV u | u < l1    = -sqrt (s / n)
           | u < l2    =  sqrt (s / n)
           | otherwise = 0
    [b11,b12,b13,b14,b15,b16,b17,b18] = take 8 v
    [b21,b22,b23,b24,b25,b26,b27,b28] = drop 8 v
    b1 = (b11,b12,b13,b14,b15,b16,b17,b18)
    b2 = (b21,b22,b23,b24,b25,b26,b27,b28)
    d = b11*b21 + b12*b22 + b13*b23 + b14*b24 + b15*b25 + b16*b26 + b17*b27 + b18*b28

randomProject :: (HuMoments,HuMoments) -> HuMoments -> (Float,Float)
randomProject (b1,b2) h = (realToFrac x, realToFrac y)
  where
    (b11,b12,b13,b14,b15,b16,b17,b18) = b1
    (b21,b22,b23,b24,b25,b26,b27,b28) = b2
    (h1,h2,h3,h4,h5,h6,h7,h8) = h
    x = b11*h1 + b12*h2 + b13*h3 + b14*h4 + b15*h5 + b16*h6 + b17*h7 + b18*h8
    y = b21*h1 + b22*h2 + b23*h3 + b24*h4 + b25*h5 + b26*h6 + b27*h7 + b28*h8

-- moment covariance matrix : | c20 c11 |
--                            | c11 c02 |
-- det: c20*c02 - c11^2
-- tra: c20+c02

ellipse :: CentralMoments -> ((Int,Int),(Int,Int),Float)
ellipse (c00,cx,cy,c11,c20,c02,_,_,_,_) =
  ((round cx, round cy),(round r1, round r2),realToFrac $ 180*a)
  where
    --cx = m10 / m00
    --cy = m01 / m00
    c20' = c20 / c00 -- m20 / m00 - cx**2
    c11' = c11 / c00 -- m11 / m00 - cx*cy
    c02' = c02 / c00 -- m02 / m00 - cy**2
    -- m = M.fromRows [V.fromList[c20,c11],V.fromList[c11,c02]]
    -- (val,vec) = eigSH m
    -- [v1,v2] = toList val
    -- axes = toColumns vec
    -- [m1,m2] = V.toList $ head axes
    det = c20' * c02' - c11'^2
    tra = c20' + c02'
    base = tra / 2
    mod = sqrt $ tra**2 / 4 - det
    l1 :: Double
    --l1 = 0.5 * (c20 - c02) + 0.5 * (sqrt $ 4 * c11**2 + (c20 - c02)**2)
    l1 = base + mod
    l2 :: Double
    --l2 = 0.5 * (c20 - c02) - 0.5 * (sqrt $ 4 * c11**2 + (c20 - c02)**2)
    l2 = base - mod
    e = sqrt $ 1 - (min l1 l2) / (max l1 l2)
    --e = sqrt $ 1 - (v2/v1)
    r1 = sqrt $ c00 / (pi * e)
    r2 = e * r1
    a :: Double
    dx = (max l1 l2) - c02'
    --dx = m1
    dy = c11'
    --dy = m2
    --a = 0.5 * (atan2 (2 * c11) (c20 - c02))
    a = atan2 ((sign dx) * dy) ((sign dx) * dx)
    sign x | x < 0 = -1
           | otherwise = 1

drawEllipse :: ((Int,Int),(Int,Int),Float) -> Image RGB Float
    -> Image RGB Float
drawEllipse (p,r,a) image =
  image
    <# ellipseOp red 1 p r a (0,360)
    <# circleOp red p 3 (Stroked 1)

-- | Get filenames with a given extension in the given path
getFiles :: String -> FilePath -> IO [FilePath]
getFiles ext path = map (\f -> path ++ f) <$>
  filter (\f -> takeExtension f == ext) <$> getDirectoryContents path
--map (snd.splitFileName) <$>

addPath f = return $ "./shapes/" ++ f

huFromFile :: Int -> Bool -> FilePath -> IO HuMoments
huFromFile nbins inv p = do
  print p
  i <- readFromFile p
  let
    hist = accHistogram nbins $ getValues i
    t = tOtsu hist
    m | inv = (1,0)
      | otherwise = (0,1)
    timg = threshold m t i
  return $! huMoments $! centralMoments $! rawMoments timg

--readSamples :: IO [Image GrayScale Float]
--readSamples = mapM readFromFile samples

type HuFeature = (Int,HuMoments)

instance KD.Point HuFeature where
  dimension h = 8
  coord 0 (_,(h1,h2,h3,h4,h5,h6,h7,h8)) = h1
  coord 1 (_,(h1,h2,h3,h4,h5,h6,h7,h8)) = h2
  coord 2 (_,(h1,h2,h3,h4,h5,h6,h7,h8)) = h3
  coord 3 (_,(h1,h2,h3,h4,h5,h6,h7,h8)) = h4
  coord 4 (_,(h1,h2,h3,h4,h5,h6,h7,h8)) = h5
  coord 5 (_,(h1,h2,h3,h4,h5,h6,h7,h8)) = h6
  coord 6 (_,(h1,h2,h3,h4,h5,h6,h7,h8)) = h7
  coord 7 (_,(h1,h2,h3,h4,h5,h6,h7,h8)) = h8
  coord _ (_,(h1,h2,h3,h4,h5,h6,h7,h8)) = 1000000000
  dist2 (_,(h11,h12,h13,h14,h15,h16,h17,h18)) (_,(h21,h22,h23,h24,h25,h26,h27,h28)) =
    (h11-h21)**2 + (h12-h22)**2 + (h13-h23)**2 + (h14-h24)**2 + (h15-h25)**2 +
      (h16-h26)**2 + (h17-h27)**2 + (h18-h28)**2

huToVec :: HuFeature -> [Float]
huToVec (_,(h1,h2,h3,h4,h5,h6,h7,h8)) = map realToFrac [h1,h2,h3,h4,h5,h6,h7,h8]

-- plot width in pixels
width = 210
-- plot height in pixels
height = 40
-- plot margin
margin = 5
-- plot x scale

knear kd p = map fst $ KD.kNearestNeighbors kd 5 p

readData :: FilePath -> IO [HuFeature]
readData p = do
  cb <- B.readFile p
  case S.decode cb of
    Left e -> error e
    Right cs -> return cs

mapM' :: Monad m => (a-> m b) -> [a] -> m [b]
mapM' _ []     = return []
mapM' f (x:xs) = do
  y  <- f x
  ys <- y `seq` mapM' f xs
  return (y:ys)

main = do
  (nbins,inv,inputImage, outputImage) <- readArgs
  --cimages <- getFiles ".png" "./shapes/circles/"
  --chu <- mapM' (huFromFile nbins inv) $ L.sort cimages
  cf <- readData "circles.dat"
  --qimages <- getFiles ".png" "./shapes/quads/"
  --qhu <- mapM' (huFromFile nbins inv) $ L.sort qimages
  qf <- readData "quads.dat"
  --timages <- getFiles ".png" "./shapes/triangles/"
  --thu <- mapM' (huFromFile nbins inv) $ L.sort timages
  tf <- readData "triangles.dat"
  img <- readFromFile inputImage
  let
    dat = (cf ++ qf ++ tf)
    vdat = map huToVec dat
    xmins = map minimum vdat
    xmaxs = map maximum vdat
    --cf :: [HuFeature]
    --cf = zip (repeat 0) chu
    chist = accBoundedVectorHistogram (xmins,xmaxs) 300 $ map huToVec cf
    --qf :: [HuFeature]
    --qf = zip (repeat 1) qhu
    qhist = accBoundedVectorHistogram (xmins,xmaxs) 300 $ map huToVec qf
    --tf :: [HuFeature]
    --tf = zip (repeat 2) thu
    thist = accBoundedVectorHistogram (xmins,xmaxs) 300 $ map huToVec tf
    ctest = take 100 cf
    ctrain = drop 100 cf
    qtest = take 100 qf
    qtrain = drop 100 qf
    ttest = take 100 tf
    ttrain = drop 100 tf
    kd = KD.fromList (ctrain ++ qtrain ++ ttrain)
    --b = sparseRandomBasis
    b = gaussianRandomBasis
    cran = map ((randomProject b).snd) cf
    qran = map ((randomProject b).snd) qf
    tran = map ((randomProject b).snd) tf
    xs = map (realToFrac.fst) (cran ++ qran ++ tran)
    ys = map (realToFrac.snd) (cran ++ qran ++ tran)
    xscale = (max (abs $ minimum xs) (maximum xs)) * 2
    -- plot y scale
    yscale = (max (abs $ minimum ys) (maximum ys)) * 2
    -- y value minimum
    ymin = -(max (abs $ minimum ys) (maximum ys))
    cps = signalToPixel (600,600) margin (xscale,yscale) ymin cran
    qps = signalToPixel (600,600) margin (xscale,yscale) ymin qran
    tps = signalToPixel (600,600) margin (xscale,yscale) ymin tran
    gimg = gaussian (5,5) img
    hist = accHistogram nbins $ getValues gimg
    t = tOtsu hist
    m | inv = (1,0)
      | otherwise = (0,1)
    timg = threshold m t gimg
    r = rawMoments timg
    c = centralMoments r
    e = ellipse c
    clear = emptyColorImage (width,height) white
  --B.writeFile "circles.dat" $ S.encode cf
  --B.writeFile "quads.dat" $ S.encode qf
  --B.writeFile "triangles.dat" $ S.encode tf
  --print (length cf,ctest)
  --print (length qf,qtest)
  --print (length tf,ttest)
  --print ttest
  print $ map (\xs -> (head xs, length xs)) $ group $ L.sort $ concatMap (knear kd) ctest
  print $ map (\xs -> (head xs, length xs)) $ group $ L.sort $ concatMap (knear kd) qtest
  print $ map (\xs -> (head xs, length xs)) $ group $ L.sort $ concatMap (knear kd) ttest
  let
      cimgs = map (\h -> plotHistogram black margin h clear) chist
      qimgs = map (\h -> plotHistogram black margin h clear) qhist
      timgs = map (\h -> plotHistogram black margin h clear) thist
  saveImage "hist.png" $ montage (4,6) 2 $
    take 4 cimgs ++ take 4 qimgs ++ take 4 timgs ++
    drop 4 cimgs ++ drop 4 qimgs ++ drop 4 timgs
  saveImage outputImage $ drawEllipse e $ convGrayToColor timg
  --print (xscale,yscale,ymin)
  saveImage "plot.png" $
    plotPoints red 5 cps $
    plotPoints green 5 qps $
    plotPoints blue 5 tps $
    emptyColorImage (600,600) white
