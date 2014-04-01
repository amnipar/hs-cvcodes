module IOUtils
( readGrayImage
, readColorImage
, getFilesInPath
) where

import CV.Image

import Data.List
import Control.Monad
import Control.Applicative hiding (empty)
import System.Directory
import System.FilePath

-- | Reads a grayscale image from a file; can be used to disambiguate the type
--   associated with readFromFile.
readGrayImage :: String -> IO (Image GrayScale D32)
readGrayImage = readFromFile

-- | Reads a color image from a file; can be used to disambiguate the type
--   associated with readFromFile.
readColorImage :: String -> IO (Image RGB D32)
readColorImage = readFromFile

-- | Get filenames with a given extension in the given path, sorted by name.
--   The filenames will include the path also.
getFilesInPath :: FilePath -> String -> IO [FilePath]
getFilesInPath path ext = map ((++)path) <$> sort <$>
  filter (\f -> takeExtension f == ext) <$> getDirectoryContents path
