module Main where

import CV.Image

import Images
import Histogram
import Thresholding
import Moments

import Control.Monad
import Control.Applicative hiding (empty)
import Data.List as L
import qualified Data.ByteString as B
import Data.Serialize as S
import System.Directory
import System.FilePath
import ReadArgs

huFromFile :: Int -> Bool -> FilePath -> IO HuMoments
huFromFile nbins inv p = do
  print p
  i <- readFromFile p
  let
    hist = accHistogram nbins $ getValues i
    t = tOtsu hist
    m | inv = (1,0)
      | otherwise = (0,1)
    timg = threshold m t i
  return $! huMoments $! centralMoments $! rawMoments timg

mapM' :: Monad m => (a-> m b) -> [a] -> m [b]
mapM' _ []     = return []
mapM' f (x:xs) = do
  y  <- f x
  ys <- y `seq` mapM' f xs
  return (y:ys)

-- | Get filenames with a given extension in the given path
getFiles :: String -> FilePath -> IO [FilePath]
getFiles ext path = map (\f -> path ++ f) <$>
  filter (\f -> takeExtension f == ext) <$> getDirectoryContents path
--map (snd.splitFileName) <$>

main = do
  (path,nbins,inv,n,file) <- readArgs
  images <- getFiles ".png" path
  hu <- mapM' (huFromFile nbins inv) $ take n $ L.sort images
  let
    f :: [HuFeature]
    f = zip (repeat 0) hu
  B.writeFile file $ S.encode f
