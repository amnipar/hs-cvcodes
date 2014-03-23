module IOUtils
( readGrayImage
, readColorImage
) where

import CV.Image

readGrayImage :: String -> IO (Image GrayScale D32)
readGrayImage = readFromFile

readColorImage :: String -> IO (Image RGB D32)
readColorImage = readFromFile
