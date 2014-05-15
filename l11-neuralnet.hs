module Main where

import AI.ScomaNN.MLP as MLP

import Control.Monad
import Control.Applicative
import System.Random.MWC
import System.Random
import System.Random.Shuffle
import System.Directory
import System.FilePath
import qualified Data.ByteString as B
import Data.Serialize as S
import Data.List as L
import Data.Ord
import Text.Printf

import BasicUtils
import Moments
import Random

randomShuffle :: [a] -> [a]
randomShuffle is = shuffle' is (length is) (mkStdGen 41234)

huToFeat (c,(h1,h2,h3,h4,h5,h6,h7,h8)) =
  ([h1,h2,h3,h4,h5,h6,h7,h8],cToVec c)
  where
    cToVec 0 = [1,0,0]
    cToVec 1 = [0,1,0]
    cToVec 2 = [0,0,1]

readData :: FilePath -> IO [HuFeature]
readData p = do
  cb <- B.readFile p
  case S.decode cb of
    Left e -> error e
    Right cs -> return cs

runMLP :: MLP.MLP -> ([Double],[Double]) -> (Int,Bool,Double)
runMLP mlp (input,target) = check (findBest $ MLP.feed mlp input, findBest $ target)
  where
    check ((c1,v1):(_,v2):_, (c2,_):_) = (c2,c1==c2,v1-v2)
    findBest o = reverse $ L.sortBy (comparing snd) $ zip [0..] o
    thresh = map t
    t :: Double -> Int
    t v | v > 0.3   = 1
        | otherwise = 0

summarize rs = map (norm.(foldr combine (0,0,0,0))) $ 
    groupBy (fst3eq) $ sortBy (comparing fst3) rs
  where
    fst3 (a,_,_) = a
    fst3eq (a1,_,_) (a2,_,_) = a1 == a2
    combine (c,t,d) (_,tp,n,td)
      | t         = (c,tp+1,n+1,td+d)
      | otherwise = (c,tp,  n+1,td+d)
    norm (c,tp,n,td) = (c,tp,n,td / (fromIntegral n))

bestRandomStart m r nstarts traindata = do
  let
    seeds = map round $ getUniformVector 1 1000000 nstarts
    inputs = map fst traindata
    targets = map snd traindata
    weights = replicate (length inputs) [1]
    config s = MLPConfig [8,m,3] s r 0.00001 10000
    configs = map config seeds
    trainMLP i o w c = MLP.train c i o w
  minimumBy (comparing snd) <$> mapM (trainMLP inputs targets weights) configs

replaceClass c (_,f) = (c,f)

testLayers r nstarts traindata testdata m = do
  (mlp,fval) <- bestRandomStart m r nstarts traindata
  print m
  print fval
  print $ summarize $ map (runMLP mlp) testdata

testReg m nstarts traindata testdata r = do
  (mlp,fval) <- bestRandomStart m r nstarts traindata
  print r
  print fval
  print $ summarize $ map (runMLP mlp) testdata
  tval <- MLP.test mlp (map fst testdata) (map snd testdata)
      (replicate (length testdata) [1])
  print tval

main = do
  cf <- readData "circles.dat"
  qf <- readData "quads.dat"
  tf <- readData "triangles.dat"
  let
    ctest = take 25 cf
    ctrain = drop 25 cf
    qtest = take 25 qf
    qtrain = drop 25 qf
    ttest = take 25 tf
    ttrain = drop 25 tf
    traindata :: [([Double],[Double])]
    traindata = randomShuffle $ map huToFeat (ctrain ++ qtrain ++ ttrain)
    testdata :: [([Double],[Double])]
    testdata = map huToFeat (ctest ++ qtest ++ ttest)
  -- for running individual tests
  --(mlp,fval) <- bestRandomStart 1 0.001 1 traindata
  --print fval
  --tval <- MLP.test mlp (map fst testdata) (map snd testdata)
  --    (replicate (length testdata) [1])
  --print tval
  -- for finding the best layer size
  --mapM_ (testLayers 0.001 5 traindata testdata) $
  --    [1,2,3,4,5,6,7,8]
      -- [8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24]
  -- for finding the best regularization coefficient
  mapM_ (testReg 5 5 traindata testdata) $
      [0.5,0.1,0.05,0.01,0.005,0.001,0.0005,0.0001]
