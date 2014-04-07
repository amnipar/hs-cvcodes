{-#LANGUAGE ParallelListComp#-}
-- | Original code by Ville Tirronen. Under construction.
module Main where

import qualified Data.Packed.Vector as V
import Numeric.LinearAlgebra as LA
import CV.Image
import CV.ColourUtils
import CV.Transforms
import System.IO.Unsafe
import Data.Function
import Data.List

import Control.Monad
import Control.Applicative hiding (empty)
import System.Directory
import System.FilePath

-- | Get filenames with a given extension in the given path
getFiles :: String -> FilePath -> IO [FilePath]
getFiles ext path = map (\f -> "./uwa-faces/" ++ f) <$>
  filter (\f -> takeExtension f == ext) <$> getDirectoryContents path
--map (snd.splitFileName) <$> 

addPath f = return $ "./uwa-faces/" ++ f

readSamples :: IO [Image GrayScale Float]
readSamples = mapM readFromFile samples

main = do
   faces <- getFiles ".png" "./uwa-faces"
   allfaces <- (mapM loadAsVector faces)
   sfaces <- readSamples
   saveImage "faces.png" $ montage (4,4) 2 sfaces
   Just single <- (loadImage "kohdekasvot.png")
   let
       mean = (sum allfaces) * (fromIntegral (length allfaces))
       φ = map (\x -> x-mean) allfaces
       (w,h)   = getSize single
       s = fromColumns φ
       l = trans s <> s
       (λ,e) = eigSH l
       eigenface :: Vector Double -> Vector Double
       eigenface ev = sum [LA.scale e φ_i | φ_i <- φ | e <- V.toList ev]
       eigenfaces = map eigenface (toColumns e)

       -- Vektoreiden projisoiminen 'kasvoavaruuteen'
       project :: Vector Double -> Vector Double
       project t = let m = t - mean in V.fromList [e `dot` m | e <- eigenfaces]

       -- Kantavektoreiden esittäminen kuvina
       ef 0 = renderVector (w,h) mean
       ef n = renderVector (w,h) . (!! n) $ eigenfaces

       target_φ = pixelsOf (stretchHistogram single)
       target_p = project target_φ
       projected = map project allfaces
       distances = sortBy (compare`on`snd) . zip faces
                                           . map (\i -> norm² (i - target_p))
                                           $ projected
       bestFit = take 15 . map fst $ distances
   print target_p
   print (project $ (allfaces !! 2) - mean)
   mapM_ (print) distances
   best <- mapM readFromFile bestFit
   
   saveImage "eface.png" . montage (4,4) 2 $ (map (stretchHistogram . ef) [0..15])
   saveImage "bestfit.png" . montage (4,4) 2 $ single:best

-- | Tuttu etäisyys
norm² :: Vector Double -> Double
norm² v = v `dot` v

-- Kuvan lataus vektoriksi
loadAsVector face = do
   print face
   Just x <- loadImage $ face
   return (pixelsOf $ stretchHistogram x)

-- | Apufunktio pikseleiden irroitukseen kuvista
pixelsOf :: Image GrayScale D32 -> Vector Double
pixelsOf im = V.fromList [realToFrac (getPixel (i,j) im) | i<-[0,1..w-1],j<-[0,1..h-1]]
    where (w,h) = getSize im

-- | Apufunktio, joka muuttaa vektorin kuvaksi.
renderVector :: (Int,Int) -> V.Vector Double -> Image GrayScale Float
renderVector (w,h) vec = unsafePerformIO $ do
  img <- toMutable $ empty (w,h)
  sequence_ [(setPixel (x,y) (realToFrac e) img)
             | e <- V.toList vec
             | (x,y) <- cross [0..w-1] [0..h-1]]
  fromMutable(img)

cross xs ys = [(x,y) | x <- xs , y <- ys]

samples = 
  [ "./uwa-faces/sub1_session1_frame1.png"
  , "./uwa-faces/sub2_session1_frame1.png"
  , "./uwa-faces/sub3_session1_frame1.png"
  , "./uwa-faces/sub4_session1_frame1.png"
  , "./uwa-faces/sub5_session1_frame1.png"
  , "./uwa-faces/sub6_session1_frame1.png"
  , "./uwa-faces/sub7_session1_frame1.png"
  , "./uwa-faces/sub8_session1_frame1.png"
  , "./uwa-faces/sub9_session1_frame1.png"
  , "./uwa-faces/sub10_session1_frame1.png"
  , "./uwa-faces/sub11_session1_frame1.png"
  , "./uwa-faces/sub14_session1_frame1.png"
  , "./uwa-faces/sub16_session1_frame1.png"
  , "./uwa-faces/sub17_session1_frame1.png"
  , "./uwa-faces/sub18_session1_frame1.png"
  , "./uwa-faces/sub19_session1_frame1.png" ]
