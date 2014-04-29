module Main where

import CV.Image
import CV.Filters
import qualified CV.ImageMath as IM
import CV.ImageMathOp
import CV.Operations
import CV.Matrix as M

import DrawingUtils
import Images
import Filters
import Gaussian
import Neighborhoods
import Thresholding

import ReadArgs

-- recommended values are from 3 to 6
sigmaMultiplier = 3

-- recommended to fit at least 3 sigma into the mask radius
sigmaToSize sigma = 2 * (ceiling $ sigmaMultiplier * sigma) + 1

sigmaToMask f sigma = createMask2D (f sigma) (sigmaToSize sigma)

sigmaToFilter f sigma = convolve2D mask center
  where
    mask = createMask2D (f sigma) size
    size = sigmaToSize sigma
    center = getMaskCenter2D size

-- sigma of the gaussian is sqrt t, where t is 0,2,4,... and 0 is original
--sigmas = [sqrt 2, sqrt 4, sqrt 8, sqrt 16, sqrt 32]
isigmas = [1,1.6,1.6**2,1.6**3,1.6**4]
-- derivative sigma is one bigger than integral sigma
dsigmas = map (+1) (0:isigmas)
sizes = map sigmaToSize isigmas
smasks = map (sigmaToMask gaussian2D) isigmas
sfilters = map (sigmaToFilter gaussian2D) isigmas
dxmasks = map (sigmaToMask gaussian2Ddx) dsigmas
dxfilters = map (sigmaToFilter gaussian2Ddx) dsigmas
dymasks = map (sigmaToMask gaussian2Ddy) dsigmas
dyfilters = map (sigmaToFilter gaussian2Ddy) dsigmas
dx2masks = map (sigmaToMask gaussian2Ddx2) dsigmas
dx2filters = map (sigmaToFilter gaussian2Ddx2) dsigmas
dy2masks = map (sigmaToMask gaussian2Ddy2) dsigmas
dy2filters = map (sigmaToFilter gaussian2Ddy2) dsigmas
dxdymasks = map (sigmaToMask gaussian2Ddxdy) dsigmas
dxdyfilters = map (sigmaToFilter gaussian2Ddxdy) dsigmas

crossesZero (_,ns) = (minimum ns) < 0 && (maximum ns) > 0

crossesZeroMax ((v,ns1),(_,ns2)) =
  (minimum ns2) < 0 && (maximum ns2) > 0 && maxEdge (v,ns1)

zeroCrossings image =
  filterNeighborhood n8 crossesZero image

maxZeroCrossings mag ang log =
  filterNeighborhoodPair ((nes5 ang),ns5) crossesZeroMax (mag,log)

valueToMax ((x,y),_) = ((x,y),1)

filterImage img f = f img

zeroNormalize image = norm |* (iext |+ image)
  where
    (imin,imax) = IM.findMinMax image
    iext = max (abs imin) imax
    norm = 1 / (2 * iext)

derivativesToGradient (dx,dy) = (mag,ang)
  where
    mag = IM.sqrt $ (dx #* dx) #+ (dy #* dy)
    ang = quantizeAngle4 $ IM.atan2 dy $ preventZero dx

maxEdge (v,[]) = False
maxEdge (v,ns) = v >= (maximum ns)

gradientExtrema mag ang =
  filterNeighborhood (nes5 ang) maxEdge mag

valueToColor c ((x,y),_) = ((x,y),c)

drawGradientExtrema ((mag,ang), img) = drawPixelsColor (grayToRGB img) $
  map (valueToColor cyan) $ gradientExtrema mag ang

drawZeroCrossings ((mag,ang), log, img) = drawPixelsColor (grayToRGB img) $
  map (valueToColor cyan) $ maxZeroCrossings mag ang log

mapPair f a (b,c) = (f a b, f a c)

main = do
  (sourceImage,targetImage) <- readArgs
  img <- readFromFile sourceImage
  let
    (w,h) = getSize img
    clear = emptyGrayImage (w,h) 0
    scales = map (filterImage img) sfilters
    dximages = zipWith (|*) dsigmas $ map (filterImage img) dxfilters
    dyimages = zipWith (|*) dsigmas $ map (filterImage img) dyfilters
    gradients = map derivativesToGradient $ zip dximages dyimages
    tmags = map ((threshold (0,1) 0.05).fst) gradients
    gradients' = zipWith (mapPair (#*)) tmags gradients
    dx2images = map (filterImage img) dx2filters
    dy2images = map (filterImage img) dy2filters
    dxdyimages = map (filterImage img) dxdyfilters
    logimages = zipWith (#+) dx2images dy2images
    laplacians = zipWith (#-) (img:scales) scales
    zeros = map ((drawPixelsGray clear).(map valueToMax).zeroCrossings) laplacians
    sums = foldr (#+) (last scales) laplacians
  saveImage targetImage $ montage (6,8) 2 $
       ([grayToRGB img] ++ (map grayToRGB scales) ++
       (map drawGradientExtrema $ zip gradients' (map zeroNormalize dximages)) ++
       (map drawGradientExtrema $ zip gradients' (map zeroNormalize dyimages)) ++
       (map drawZeroCrossings $ zip3 gradients' logimages (map zeroNormalize dx2images)) ++
       (map drawZeroCrossings $ zip3 gradients' logimages (map zeroNormalize dy2images)) ++
       (map drawZeroCrossings $ zip3 gradients' logimages (map zeroNormalize dxdyimages)) ++
       (map drawZeroCrossings $ zip3 gradients' logimages (map zeroNormalize logimages)) ++
       [grayToRGB clear] ++ (map (grayToRGB.zeroNormalize) laplacians))
