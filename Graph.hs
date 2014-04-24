{-#LANGUAGE TypeFamilies#-}
module Graph
( findConnectedComponents
, findSpanningForest
-- , findWatersheds
) where

import CVSU.Types
import CVSU.PixelImage hiding (writePixelImage)
import CVSU.QuadForest
import CVSU.Graph
import CVSU.Set
import CVSU.Attribute

import CV.Image
import CV.Filters
import CV.Thresholding
import CV.ColourUtils
import CV.CVSU
import CV.CVSU.Drawing

import System.IO.Unsafe
import Data.List
import Data.Ord

valueAttribute :: IO (Attribute Int)
valueAttribute = attributeCreate 1 0

componentAttribute :: IO (Attribute Set)
componentAttribute = do
  s <- setCreate
  attributeCreate 2 s

-- takes a label value and a node.
-- if node has same value as neighbor, makes a union with neighbor
-- using the setAttr attribute for storing the set membership.
-- if has different value, creates a new set and uses the setLabel.
-- returns the new setLabel and the new Node.
unionWithSimilarNeighbors :: (Eq a, AttribValue a, AttribValue b) =>
    Attribute (a,Set) -> Node b -> IO ()
unionWithSimilarNeighbors pairAttr node = do
  (val,set) <- getAttribute pairAttr node
  neighbors <- nodeNeighbors node
  mapM_ (unionWithSimilar pairAttr val set) neighbors
  where
    unionWithSimilar pair val set node = do
      (nval,nset) <- getAttribute pair node
      if (val == nval)
         then do
           setUnion set nset
         else do
           setNull

removeLinkSmaller :: Attribute Set -> Double -> Link -> IO ()
removeLinkSmaller setAttr t link =
  if linkWeight link < t
    then do
      set1 <- getAttribute setAttr $ linkFrom link
      set2 <- getAttribute setAttr $ linkTo link
      if setId set1 /= setId set2
        then do
          setUnion set1 set2
          return ()
        else return ()
    else return ()

connectedComponents :: (Eq a, AttribValue a, AttribValue b) =>
    Attribute a -> Attribute Set -> Graph b -> IO (Graph (a,Set))
connectedComponents valueAttr setAttr graph = do
  pairAttr <- attributePair valueAttr setAttr
  mapM_ (unionWithSimilarNeighbors pairAttr) $ nodes graph
  graphGetAttribute pairAttr graph

minimumSpanningForest :: (Num a, AttribValue a, AttribValue b) =>
  Attribute a -> Attribute Set -> Double -> Graph b -> IO (Graph (a,Set))
minimumSpanningForest valueAttr setAttr t graph = do
  pairAttr <- attributePair valueAttr setAttr
  mapM_ (removeLinkSmaller setAttr t) $
      sortBy (comparing linkWeight) $ links graph
  graphGetAttribute pairAttr graph

valueGraph :: CGraph -> PixelImage -> Attribute Int -> IO (Graph Int)
valueGraph cg pimg value =
  --graphCreateFromImage pimg 5 5 8 8 Neighborhood4 value cg
  graphCreateFromImage pimg 0 0 1 1 Neighborhood4 value cg

findConnectedComponents :: Image GrayScale Float -> Image GrayScale Float
findConnectedComponents image = unsafePerformIO $ do
  pimg <- toPixelImage $ unsafeImageTo8Bit $ stretchHistogram image
  value <- valueAttribute
  comp <- componentAttribute
  cg <- newCGraph
  vgraph <- valueGraph cg pimg value
  sgraph <- graphAddAttribute comp vgraph
  cgraph <- connectedComponents value comp sgraph
  sets <- mapM (getAttribute comp) $ nodes cgraph
  let
    spicker = createGrayPicker () sets
  return $ drawGraphImageGray spicker 1 comp cgraph


findSpanningForest :: Float -> Image GrayScale Float -> Image GrayScale Float
findSpanningForest t image = unsafePerformIO $ do
  pimg <- toPixelImage $ unsafeImageTo8Bit $ stretchHistogram image
  value <- valueAttribute
  comp <- componentAttribute
  cg <- newCGraph
  vgraph <- valueGraph cg pimg value
  sgraph <- graphAddAttribute comp vgraph
  cgraph <- minimumSpanningForest value comp (realToFrac $ t*255) sgraph
  sets <- mapM (getAttribute comp) $ nodes cgraph
  let
    spicker = createGrayPicker () sets
  return $ drawGraphImageGray spicker 1 comp cgraph
