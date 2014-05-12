module Main where

import Shapes
import CV.Image
import Control.Monad
import Text.Printf

numImages = 999

writeImage :: String -> (Int,Image GrayScale Float) -> IO ()
writeImage base (i,img) = saveImage (printf (base ++ "%03d.png") i) img

main = do
  mapM_ (writeImage "./shapes/triangles/triangle") $ zip [1..] $
      generateTriangleImages numImages 201
  mapM_ (writeImage "./shapes/quads/quad") $ zip [1..] $
      generateQuadImages numImages 201
  mapM_ (writeImage "./shapes/circles/circle") $ zip [1..] $
      generateCircleImages numImages 201
