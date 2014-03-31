module Main where

import CV.Image
import qualified CV.ImageMath as IM
import CV.ImageMathOp
import CV.Filters
import CV.Operations
import CV.Pixelwise

import ReadArgs

-- cov = E((X-E(X))(Y-E(Y))) = E(XY) - E(X)E(Y)

covariance :: (Int,Int) -> Image GrayScale Float -> [Image GrayScale Float]
    -> [Image GrayScale Float]
covariance s x ys = map (cov x ex) ys
  where
    ex = gaussian s x
    cov x ex y = exy #- ex #* ey
      where
        xy = x #* y
        exy = gaussian s xy
        ey = gaussian s x

main = do
  (mode,regionSize,inputImage,outputImage) <- readArgs
  img00 <- readFromFile(inputImage)
  let
    size = (regionSize,regionSize)
    img10 = shift (1,0) img00
    img01 = shift (0,1) img00
    img11 = shift (1,1) img00
    x = img00
    x2 = x #* x
    mu1 = gaussian size x
    sigma2 = (gaussian size x2) #- (mu1 #* mu1)
    sigma1 = IM.sqrt sigma2
  case mode of
    "covariance" ->
      saveImage outputImage $ montage (2,2) 2 $ map unitNormalize $
        [x] ++ covariance size img00 [img10,img01,img11]

shift :: (Int,Int) -> Image GrayScale Float -> Image GrayScale Float
shift (sx,sy) image = imageFromFunction (w,h) f
  where
    (w,h) = getSize image
    f (x,y) | x < sx || y < sy = getPixel (x,y) image
            | otherwise        = getPixel (x-sx,y-sy) image
