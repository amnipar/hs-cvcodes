module Morphology
( morph
, dilate
, erode
, open
, close
, hitormiss
, thinning
, skeleton
, pruneEndpoints
, findPoints
, findJunctions
, crossMask
, squareMask
, skeletonMasks
, endpointMasks
, junctionMasks
) where

import CV.Image
import CV.Filters
import qualified CV.ImageMath as IM
import CV.ImageMathOp
import CV.Matrix as M
import CV.Pixelwise
import CV.ColourUtils

import Images
import DrawingUtils
import Thresholding

maskToMatrix :: [Float] -> (Matrix D32, (Int,Int))
maskToMatrix mask = (M.fromList size mask, center)
  where
    size = (s,s)
    center = (c,c)
    s :: Int
    s = round $ sqrt $ fromIntegral $ length mask
    c | even s    = (s `div` 2) - 1
      | otherwise = (s `div` 2)


updateSkeleton skeleton image open =
  imageFromFunction (w,h) (update skeleton image open)
  where
    (w,h) = getSize skeleton
    update i1 i2 i3 (x,y) | v1 == 1            = 1
                          | v2 == 1 && v3 == 0 = 1
                          | otherwise          = 0
      where
        v1 = getPixel (x,y) i1
        v2 = getPixel (x,y) i2
        v3 = getPixel (x,y) i3

simpleSkeleton :: [Float] -> Image GrayScale D32 -> Image GrayScale D32
    -> Image GrayScale D32
simpleSkeleton mask acc image
  | IM.sum eroded == 0 = acc'
  | otherwise = simpleSkeleton mask acc' eroded
  where
    eroded = erode mask 1 image
    opened = open mask 1 image
    acc' = updateSkeleton acc image opened
      --IM.minS 1 $ acc #+ (image #* ((-1) |* (1 -| opened)))

thinning :: [Float] -> Image GrayScale D32 -> Image GrayScale D32
thinning mask image = image #- (2 |* (hitormiss mask image))

skeleton image
  | (IM.sum (IM.maxS 0 thinned)) == 0 = image
  | (IM.sum (IM.maxS 0 thinned)) < (IM.sum (IM.maxS 0 image)) =
      skeleton thinned
  | otherwise = image
  where
    thinned = foldr thinning image skeletonMasks

pruneEndpoints n image
  | n <= 1 = image
  | (IM.sum (IM.maxS 0 thinned)) <= 1 = image
  | (IM.sum (IM.maxS 0 thinned)) < (IM.sum (IM.maxS 0 image)) =
      pruneEndpoints (n-1) thinned
  | otherwise = image
  where
    thinned = foldr thinning image endpointMasks

-- | Returns an image that has the hits produced by the masks marked as 1's
findPoints image masks =
  foldr (findHit image) (emptyGrayImage (w,h) 0) masks
  where
    (w,h) = getSize image
    findHit input mask output = output #+ (hitormiss mask input)

-- | Returns an image that has the junction points of a skeleton marked as 1's
findJunctions skel = findPoints skel junctionMasks

distanceTransform :: [Float] -> Image GrayScale D32 -> Image GrayScale D32
    -> Image GrayScale D32
distanceTransform mask acc image
  | IM.sum eroded == 0 = acc
  | otherwise = distanceTransform mask acc' eroded
  where
    eroded = erode mask 1 image
    acc' = acc #+ eroded

morph :: [Float] -> Float -> Image GrayScale D32 -> Image GrayScale D32
morph mask t image = threshold (0,1) t $
  convolve2D matrix center image
  where
    (matrix,center) = maskToMatrix mask

dilate :: [Float] -> Int -> Image GrayScale D32 -> Image GrayScale D32
dilate mask n image
  | n <= 1 = dilated
  | otherwise = dilate mask (n-1) dilated
  where
    dilated = morph mask 1 image

erode :: [Float] -> Int -> Image GrayScale D32 -> Image GrayScale D32
erode mask n image
  | n <= 1 = eroded
  | otherwise = erode mask (n-1) eroded
  where
    eroded = morph mask (sum mask) image

open mask n image = dilate mask n $ erode mask n image

close mask n image = erode mask n $ dilate mask n image

hitormiss :: [Float] -> Image GrayScale D32 -> Image GrayScale D32
hitormiss mask image = morph mask (sum $ map abs mask) image

crossMask :: [Float]
crossMask =
  [ 0, 1, 0
  , 1, 1, 1
  , 0, 1, 0 ]
squareMask :: [Float]
squareMask =
  [ 1, 1, 1
  , 1, 1, 1
  , 1, 1, 1 ]
skeletonEdgeMask1 :: [Float]
skeletonEdgeMask1 =
  [-1, 0, 1
  ,-1, 1, 1
  ,-1, 0, 1 ]
skeletonEdgeMask2 :: [Float]
skeletonEdgeMask2 =
  [-1,-1,-1
  , 0, 1, 0
  , 1, 1, 1 ]
skeletonEdgeMask3 :: [Float]
skeletonEdgeMask3 =
  [ 1, 0,-1
  , 1, 1,-1
  , 1, 0,-1 ]
skeletonEdgeMask4 :: [Float]
skeletonEdgeMask4 =
  [ 1, 1, 1
  , 0, 1, 0
  ,-1,-1,-1 ]
skeletonCornerMask1 :: [Float]
skeletonCornerMask1 =
  [-1,-1, 0
  ,-1, 1, 1
  , 0, 1, 0 ]
skeletonCornerMask2 :: [Float]
skeletonCornerMask2 =
  [ 0,-1,-1
  , 1, 1,-1
  , 0, 1, 0 ]
skeletonCornerMask3 :: [Float]
skeletonCornerMask3 =
  [ 0, 1, 0
  , 1, 1,-1
  , 0,-1,-1 ]
skeletonCornerMask4 :: [Float]
skeletonCornerMask4 =
  [ 0, 1, 0
  ,-1, 1, 1
  ,-1,-1, 0 ]
endpointMask1 :: [Float]
endpointMask1 =
  [ 0, 0, 0
  ,-1, 1,-1
  ,-1,-1,-1 ]
endpointMask2 :: [Float]
endpointMask2 =
  [-1,-1, 0
  ,-1, 1, 0
  ,-1,-1, 0 ]
endpointMask3 :: [Float]
endpointMask3 =
  [-1,-1,-1
  ,-1, 1,-1
  , 0, 0, 0 ]
endpointMask4 :: [Float]
endpointMask4 =
  [ 0,-1,-1
  , 0, 1,-1
  , 0,-1,-1 ]
junctionMask1 :: [Float]
junctionMask1 =
  [ 1, 0, 1
  , 0, 1, 0
  , 0, 1, 0 ]
junctionMask2 :: [Float]
junctionMask2 =
  [ 0, 1, 0
  , 0, 1, 1
  , 1, 0, 0 ]
junctionMask3 :: [Float]
junctionMask3 =
  [ 0, 0, 1
  , 1, 1, 0
  , 0, 0, 1 ]
junctionMask4 :: [Float]
junctionMask4 =
  [ 1, 0, 0
  , 0, 1, 1
  , 0, 1, 0 ]
junctionMask5 :: [Float]
junctionMask5 =
  [ 0, 1, 0
  , 0, 1, 0
  , 1, 0, 1 ]
junctionMask6 :: [Float]
junctionMask6 =
  [ 0, 0, 1
  , 1, 1, 0
  , 0, 1, 0 ]
junctionMask7 :: [Float]
junctionMask7 =
  [ 1, 0, 0
  , 0, 1, 1
  , 1, 0, 0 ]
junctionMask8 :: [Float]
junctionMask8 =
  [ 0, 1, 0
  , 1, 1, 0
  , 0, 0, 1 ]
junctionMask9 :: [Float]
junctionMask9 =
  [ 1, 0, 0
  , 0, 1, 0
  , 1, 0, 1 ]
junctionMask10 :: [Float]
junctionMask10 =
  [ 1, 0, 1
  , 0, 1, 0
  , 1, 0, 0 ]
junctionMask11 :: [Float]
junctionMask11 =
  [ 1, 0, 1
  , 0, 1, 0
  , 0, 0, 1 ]
junctionMask12 :: [Float]
junctionMask12 =
  [ 0, 0, 1
  , 0, 1, 0
  , 1, 0, 1 ]
skeletonMasks :: [[Float]]
skeletonMasks =
  [ skeletonCornerMask4
  , skeletonEdgeMask4
  , skeletonCornerMask3
  , skeletonEdgeMask3
  , skeletonCornerMask2
  , skeletonEdgeMask2
  , skeletonCornerMask1
  , skeletonEdgeMask1
  ]
endpointMasks :: [[Float]]
endpointMasks =
  [ endpointMask1
  , endpointMask2
  , endpointMask3
  , endpointMask4
  ]
junctionMasks :: [[Float]]
junctionMasks =
  [ junctionMask12
  , junctionMask11
  , junctionMask10
  , junctionMask9
  , junctionMask8
  , junctionMask7
  , junctionMask6
  , junctionMask5
  , junctionMask4
  , junctionMask3
  , junctionMask2
  , junctionMask1
  ]
