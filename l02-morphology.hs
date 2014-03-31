module Main where

import CV.Image
import CV.Filters
import qualified CV.ImageMath as IM
import CV.ImageMathOp
import CV.Matrix as M
import CV.Pixelwise
import CV.ColourUtils

import DrawingUtils

import Debug.Trace

crossMask =
  [ 0, 1, 0
  , 1, 1, 1
  , 0, 1, 0 ]
squareMask =
  [ 1, 1, 1
  , 1, 1, 1
  , 1, 1, 1 ]
  
skeletonEdgeMask1 =
  [-1, 0, 1
  ,-1, 1, 1
  ,-1, 0, 1 ]
skeletonEdgeMask2 =
  [-1,-1,-1
  , 0, 1, 0
  , 1, 1, 1 ]
skeletonEdgeMask3 =
  [ 1, 0,-1
  , 1, 1,-1
  , 1, 0,-1 ]
skeletonEdgeMask4 =
  [ 1, 1, 1
  , 0, 1, 0
  ,-1,-1,-1 ]
skeletonCornerMask1 =
  [-1,-1, 0
  ,-1, 1, 1
  , 0, 1, 0 ]
skeletonCornerMask2 =
  [ 0,-1,-1
  , 1, 1,-1
  , 0, 1, 0 ]
skeletonCornerMask3 =
  [ 0, 1, 0
  , 1, 1,-1
  , 0,-1,-1 ]
skeletonCornerMask4 =
  [ 0, 1, 0
  ,-1, 1, 1
  ,-1,-1, 0 ]

endpointMask1 =
  [ 0, 0, 0
  ,-1, 1,-1
  ,-1,-1,-1 ]
endpointMask2 =
  [-1,-1, 0
  ,-1, 1, 0
  ,-1,-1, 0 ]
endpointMask3 =
  [-1,-1,-1
  ,-1, 1,-1
  , 0, 0, 0 ]
endpointMask4 =
  [ 0,-1,-1
  , 0, 1,-1
  , 0,-1,-1 ]

junctionMask1 =
  [ 1, 0, 1
  , 0, 1, 0
  , 0, 1, 0 ]
junctionMask2 =
  [ 0, 1, 0
  , 0, 1, 1
  , 1, 0, 0 ]
junctionMask3 =
  [ 0, 0, 1
  , 1, 1, 0
  , 0, 0, 1 ]
junctionMask4 =
  [ 1, 0, 0
  , 0, 1, 1
  , 0, 1, 0 ]
junctionMask5 =
  [ 0, 1, 0
  , 0, 1, 0
  , 1, 0, 1 ]
junctionMask6 =
  [ 0, 0, 1
  , 1, 1, 0
  , 0, 1, 0 ]
junctionMask7 =
  [ 1, 0, 0
  , 0, 1, 1
  , 1, 0, 0 ]
junctionMask8 =
  [ 0, 1, 0
  , 1, 1, 0
  , 0, 0, 1 ]
junctionMask9 =
  [ 1, 0, 0
  , 0, 1, 0
  , 1, 0, 1 ]
junctionMask10 =
  [ 1, 0, 1
  , 0, 1, 0
  , 1, 0, 0 ]
junctionMask11 =
  [ 1, 0, 1
  , 0, 1, 0
  , 0, 0, 1 ]
junctionMask12 =
  [ 0, 0, 1
  , 0, 1, 0
  , 1, 0, 1 ]

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

endpointMasks =
  [ endpointMask1
  , endpointMask2
  , endpointMask3
  , endpointMask4
  ]

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

cornerMask1 =
  [-1,-1, 0
  ,-1, 1, 1
  , 0, 1, 0 ]
cornerMask2 =
  [-1,-1,-1
  , 0, 1, 0
  , 1, 1, 1 ]
cornerMask3 =
  [ 0,-1,-1
  , 1, 1,-1
  , 0, 1, 0 ]
cornerMask4 =
  [ 1, 0,-1
  , 1, 1,-1
  , 1, 0,-1 ]
cornerMask5 =
  [ 0, 1, 0
  , 1, 1,-1
  , 0,-1,-1 ]
cornerMask6 =
  [ 1, 1, 1
  , 0, 1, 0
  ,-1,-1,-1 ]
cornerMask7 =
  [ 0, 1, 0
  ,-1, 1, 1
  ,-1,-1, 0 ]
cornerMask8 =
  [-1, 0, 1
  ,-1, 1, 1
  ,-1, 0, 1 ]

data ObjectType = DarkObject | LightObject deriving (Eq)
data ThresholdMode = ZeroOne | OneZero | MinusPlus | PlusMinus deriving(Eq)

thresh :: ThresholdMode -> Float -> Image GrayScale D32 -> Image GrayScale D32
thresh mode t image
  | mode == ZeroOne   = mapImage (lessToZero  t) image
  | mode == OneZero   = mapImage (lessToOne   t) image
  | mode == MinusPlus = mapImage (lessToMinus t) image
  | otherwise         = mapImage (lessToPlus  t) image
  where
    lessToZero  t v | v < t     =  0
                    | otherwise =  1
    lessToOne   t v | v < t     =  1
                    | otherwise =  0
    lessToMinus t v | v < t     = -1
                    | otherwise =  1
    lessToPlus  t v | v < t     =  1
                    | otherwise = -1

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

junction skel mask image = image #+ (hitormiss mask skel)

findJunctions skel =
  foldr (junction skel) (emptyGrayImage (w,h) 0) junctionMasks
  where
    (w,h) = getSize skel

distanceTransform :: [Float] -> Image GrayScale D32 -> Image GrayScale D32
    -> Image GrayScale D32
distanceTransform mask acc image
  | IM.sum eroded == 0 = acc
  | otherwise = distanceTransform mask acc' eroded
  where
    eroded = erode mask 1 image
    acc' = acc #+ eroded

morph :: [Float] -> Float -> Image GrayScale D32 -> Image GrayScale D32
morph mask t image = thresh ZeroOne t $
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

main = do
  img <- readFromFile "nut.png"
  let 
    (w,h) = getSize img
    zo_img = thresh OneZero 0.5 img
    mp_img = thresh PlusMinus 0.5 img
    closed = close crossMask 3 zo_img
    -- need to create (-1,+1)-thresholded image for skeleton
    skel = pruneEndpoints 5 $ skeleton $ IM.subS (IM.mulS 2 closed) 1
  saveImage "morph.png" $ stretchHistogram $
    ((hitormiss cornerMask1 mp_img) #+ 
     (hitormiss cornerMask2 mp_img) #+
     (hitormiss cornerMask3 mp_img) #+
     (hitormiss cornerMask4 mp_img)) #+
    (zo_img #- (erode crossMask 1 zo_img))
  saveImage "skeleton.png" $ montage (5,1) 4 $
      [zo_img
      ,mp_img
      ,closed
      ,((2 |* closed) |- 1)
      ,stretchHistogram $ (IM.maxS 0 skel) #+ 
          (2 |* (findJunctions skel))
      ]
  saveImage "distance.png" $ stretchHistogram $
      distanceTransform crossMask closed closed
