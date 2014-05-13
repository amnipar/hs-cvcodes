module Main where

import CV.Image
import CV.Filters
import CV.ImageOp
import CV.ImageMathOp
import CV.Drawing
import CV.Operations
import Utils.Rectangle hiding (scale)

import BasicUtils
import DrawingUtils
import Images
import Filters
import Histogram
import Thresholding
import Neighborhoods
import Shapes
import Random
import Moments

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

-- | Get filenames with a given extension in the given path
getFiles :: String -> FilePath -> IO [FilePath]
getFiles ext path = map (\f -> path ++ f) <$>
  filter (\f -> takeExtension f == ext) <$> getDirectoryContents path
--map (snd.splitFileName) <$>

addPath f = return $ "./shapes/" ++ f

--readSamples :: IO [Image GrayScale Float]
--readSamples = mapM readFromFile samples

-- plot width in pixels
width = 210
-- plot height in pixels
height = 40
-- plot margin
margin = 5
-- plot x scale

knear kd p = map fst $ KD.kNearestNeighbors kd 15 p

readData :: FilePath -> IO [HuFeature]
readData p = do
  cb <- B.readFile p
  case S.decode cb of
    Left e -> error e
    Right cs -> return cs

stat xs = (mu, sqrt $ s2 / n - mu**2)
  where
    s1 = sum xs
    s2 = sum $ map (**2) xs
    n = fromIntegral $ length xs
    mu = s1 / n

norm ((mu,sigma),xs) = map ((/sigma).((-)mu)) xs

clamp s x | x >  s    = s
          | x < -s    = -s
          | otherwise = x

bothr (x,y) = (realToFrac x, realToFrac y)

toMom [[b11,b12,b13,b14,b15,b16,b17,b18],[b21,b22,b23,b24,b25,b26,b27,b28]]
  = ((b11,b12,b13,b14,b15,b16,b17,b18),(b21,b22,b23,b24,b25,b26,b27,b28))

drawMat :: Int -> M.Matrix Double -> Image GrayScale Float
drawMat s m =
  img <## [rectOp ((v-minval)/scale) (-1) (mkRectangle p (s,s)) | (p,v) <- ps] 
  where
    img = emptyGrayImage (w*s,h*s) 0
    r = M.toRows m
    c = map V.toList r
    w = M.cols m
    h = M.rows m
    maxval = maximum $ map snd ps
    minval = minimum $ map snd ps
    scale = maxval - minval
    is = [(x,y) | x <- [0..w-1] , y <- [0..h-1]]
    ps = map (\(x,y) -> ((x*s,y*s), realToFrac $ m @@> (x,y))) is

--hunorm [(Float,Do)] -> HuMoments -> (Float,Float,Float,Float,Float
hunorm [(m1,s1),(m2,s2),(m3,s3),(m4,s4),(m5,s5),(m6,s6),(m7,s7),(m8,s8)]
    (h1,h2,h3,h4,h5,h6,h7,h8)
    = (((rf h1)-m1)/s1,((rf h2)-m2)/s2,((rf h3)-m3)/s3,((rf h4)-m4)/s4,
       ((rf h5)-m5)/s5,((rf h6)-m6)/s6,((rf h7)-m7)/s7,((rf h8)-m8)/s8)
  where
    rf = realToFrac

bv 0 = (1,0,0,0,0,0,0,0)
bv 1 = (0,1,0,0,0,0,0,0)
bv 2 = (0,0,1,0,0,0,0,0)
bv 3 = (0,0,0,1,0,0,0,0)
bv 4 = (0,0,0,0,1,0,0,0)
bv 5 = (0,0,0,0,0,1,0,0)
bv 6 = (0,0,0,0,0,0,1,0)
bv 7 = (0,0,0,0,0,0,0,1)

pairplot cran qran tran (x,y)
  | x == y = clear
  | otherwise =
    plotPoints red 5 cps $
    plotPoints green 5 qps $
    plotPoints blue 5 tps $ clear
  where
    b = (bv x, bv y)
    clear = emptyColorImage (100,100) white
    cps = signalToPixel (100,100) 2 (4,4) (-2) $ map (randomProject b) cran
    qps = signalToPixel (100,100) 2 (4,4) (-2) $ map (randomProject b) qran
    tps = signalToPixel (100,100) 2 (4,4) (-2) $ map (randomProject b) tran

main = do
  (nbins,inv,inputImage, outputImage) <- readArgs
  cf <- readData "circles.dat"
  qf <- readData "quads.dat"
  tf <- readData "triangles.dat"
  img <- readFromFile inputImage
  let
    dat = (cf ++ qf ++ tf)
    vdat = map huToList dat
    xmins = map minimum $ L.transpose vdat
    xmaxs = map maximum $ L.transpose vdat
    stats = map stat $ L.transpose vdat
    vdat' = L.transpose $ map ((map (clamp 2)).norm) (zip stats (L.transpose vdat))
    mat = M.fromRows $ map (V.fromList.(map realToFrac)) vdat'
    l = trans mat <> mat
    (val,vec) = eigSH l
    bs = map V.toList $ take 2 $ M.toColumns vec
    chist = accBoundedVectorHistogram (xmins,xmaxs) 300 $ map huToList cf
    qhist = accBoundedVectorHistogram (xmins,xmaxs) 300 $ map huToList qf
    thist = accBoundedVectorHistogram (xmins,xmaxs) 300 $ map huToList tf
    ctest = take 25 cf
    ctrain = drop 25 cf
    qtest = take 25 qf
    qtrain = drop 25 qf
    ttest = take 25 tf
    ttrain = drop 25 tf
    kd = KD.fromList (ctrain ++ qtrain ++ ttrain)
    --b = sparseRandomBasis
    b = gaussianRandomBasis
    --b = ((1,0,0,0,0,0,0,0),(0,0,1,0,0,0,0,0))
    --b = toMom bs
    cran = map ((hunorm (map bothr stats)).snd) cf
    cran' = map (randomProject b) cran
    qran = map ((hunorm (map bothr stats)).snd) qf
    qran' = map (randomProject b) qran
    tran = map ((hunorm (map bothr stats)).snd) tf
    tran' = map (randomProject b) tran
    xs = map (realToFrac.fst) (cran' ++ qran' ++ tran')
    ys = map (realToFrac.snd) (cran' ++ qran' ++ tran')
    xscale = 4 -- (max (abs $ minimum xs) (maximum xs)) * 2
    yscale = 4 -- (max (abs $ minimum ys) (maximum ys)) * 2
    ymin = -2 -- -(max (abs $ minimum ys) (maximum ys))
    cps = signalToPixel (600,600) margin (xscale,yscale) ymin cran' 
    qps = signalToPixel (600,600) margin (xscale,yscale) ymin qran'
    tps = signalToPixel (600,600) margin (xscale,yscale) ymin tran'
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
  --print xmins
  --print xmaxs
  print $ map (\xs -> (head xs, length xs)) $ group $ L.sort $ 
      concatMap (knear kd) ctest
  print $ map (\xs -> (head xs, length xs)) $ group $ L.sort $ 
      concatMap (knear kd) qtest
  print $ map (\xs -> (head xs, length xs)) $ group $ L.sort $ 
      concatMap (knear kd) ttest
  let
    cimgs = map (\h -> plotHistogram black margin h clear) chist
    qimgs = map (\h -> plotHistogram black margin h clear) qhist
    timgs = map (\h -> plotHistogram black margin h clear) thist
  saveImage "hist.png" $ montage (4,6) 2 $
    take 4 cimgs ++ take 4 qimgs ++ take 4 timgs ++
    drop 4 cimgs ++ drop 4 qimgs ++ drop 4 timgs
  saveImage outputImage $ drawEllipse e $ convGrayToColor timg
  --print (xscale,yscale,ymin)
  saveImage "cov.png" $ drawMat 30 l
  saveImage "plot.png" $
    plotPoints red 5 cps $
    plotPoints green 5 qps $
    plotPoints blue 5 tps $
    emptyColorImage (600,600) white
  saveImage "plots.png" $ montage (8,8) 2 $
    map (pairplot cran qran tran) [(x,y) | x <- [0..7] , y <- [0..7]]
