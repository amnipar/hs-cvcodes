module Main where

import CV.Image
import qualified CV.ImageMath as IM
import CV.ImageMathOp

import Images
import DrawingUtils
import Thresholding
import Morphology

main = do
  img <- readFromFile "bolt.png"
  let 
    (w,h) = getSize img
    thresholded = threshold (1,0) 0.5 img
    closed = close crossMask 3 thresholded
    edges = closed #- (erode squareMask 1 closed)
    skel = skeleton $ convZeroOneToMinusPlus closed
    skelEdge = ((IM.maxS 0 skel) #+ edges)
    pruned = pruneEndpoints 5 skel
    junctionPoints = map (pointToCircle 3) $
        collectPoints 0.5 $ findJunctions skel
    endPoints = map (pointToRect 2) $
        collectPoints 0.5 $ findPoints skel endpointMasks
  saveImage "skeleton.png" $ montage (2,2) 2 $
      [ convGrayToColor thresholded
      , convGrayToColor closed
      , convGrayToColor skelEdge
      , plotCircles red 0 junctionPoints $
        plotRects blue 0 endPoints $
        convGrayToColor skelEdge
      ]
