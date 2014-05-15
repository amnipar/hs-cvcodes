module Main where

import Moments

import Data.List as L
import qualified Data.ByteString as B
import Data.Serialize as S
import Data.Trees.KdTree as KD

knear kd k p = map fst $ KD.kNearestNeighbors kd k p

readData :: FilePath -> IO [HuFeature]
readData p = do
  cb <- B.readFile p
  case S.decode cb of
    Left e -> error e
    Right cs -> return cs

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
    kd = KD.fromList (ctrain ++ qtrain ++ ttrain)
    k = 15
  print $ map (\xs -> (head xs, length xs)) $ group $ L.sort $ 
      concatMap (knear kd k) ctest
  print $ map (\xs -> (head xs, length xs)) $ group $ L.sort $ 
      concatMap (knear kd k) qtest
  print $ map (\xs -> (head xs, length xs)) $ group $ L.sort $ 
      concatMap (knear kd k) ttest
