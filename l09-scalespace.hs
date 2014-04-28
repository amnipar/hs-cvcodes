module Main where

import CV.Image
import CV.Filters
import CV.ImageMath hiding (sqrt,sum)
import CV.ImageMathOp
import CV.Operations
import CV.Matrix as M

import DrawingUtils
import Images
import Filters
import Gaussian
import Neighborhoods

import ReadArgs
import Control.Monad
import System.IO.Unsafe

-- recommended values are from 3 to 6
sigmaMultiplier = 3

-- recommended to fit at least 3 sigma into the mask radius
sigmaToSize sigma = 2 * (round $ sigmaMultiplier * sigma + 1) + 1

sigmaToMask sigma = createMask2D (gaussian2D sigma) (sigmaToSize sigma)

sigmaToFilter sigma = convolve2D mask center
  where
    mask = createMask2D (gaussian2D sigma) size
    size = sigmaToSize sigma
    center = getMaskCenter2D size

-- sigma of the gaussian is sqrt t, where t is 0,1,2,... and 0 is original
--sigmas = [sqrt 2, sqrt 4, sqrt 8, sqrt 16, sqrt 32]
sigmas = [1,1.6,1.6**2,1.6**3,1.6**4]
sizes = map sigmaToSize sigmas
masks = map sigmaToMask sigmas
filters = map sigmaToFilter sigmas

crossesZero (_,ns) = (minimum ns) < 0 && (maximum ns) > 0

zeroCrossings image =
  filterNeighborhood n8 crossesZero image

valueToMax ((x,y),_) = ((x,y),1)

drawPixelsGray :: Image GrayScale Float -> [((Int,Int),Float)]
    -> Image GrayScale Float
drawPixelsGray img points = unsafePerformIO $ do
  mimg <- toMutable img
  forM_ points $ \(p,v) -> setPixel p v mimg
  fromMutable mimg

main = do
  (sourceImage,targetImage) <- readArgs
  img <- readFromFile sourceImage
  let
    (w,h) = getSize img
    clear = emptyGrayImage (w,h) 0
    scales = map (\f -> f img) filters
    laplacians = zipWith (#-) (img:scales) scales
    zeros = map ((drawPixelsGray clear).(map valueToMax).zeroCrossings) laplacians
    sums = foldr (#+) (last scales) laplacians
  saveImage targetImage $ montage (6,3) 2 $
      ([img] ++ scales ++ [clear] ++ (map unitNormalize laplacians) ++ [sums] ++ zeros)
