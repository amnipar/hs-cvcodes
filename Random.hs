module Random
( addGaussian
, generateNGaussians
, generateGaussians
, corruptSignalWithGaussian
, corruptListWithGaussian
, corruptPairsWithGaussian
, corruptWithGaussian
, newRNG
, setSeed
, mt19937
, getGaussian
, getGaussianVector
, getGaussianVectorSeeded
, getUniformVector
, getUniformVectorSeeded
) where

import Control.Monad
import GSL.Random.Gen
import GSL.Random.Dist
import System.IO.Unsafe
import Data.Time.Clock
import Foreign(Word64)

addGaussian :: RNG -> Float -> Float -> IO Float
addGaussian rng sigma x = do
  g <- liftM realToFrac $ getGaussian rng (realToFrac sigma)
  return $ x + g

generateNGaussians :: RNG -> Float -> Int -> IO [Float]
generateNGaussians _ _ 0 = return []
generateNGaussians rng sigma n = do
  g <- liftM realToFrac $ getGaussian rng (realToFrac sigma)
  gs <- generateNGaussians rng sigma (n-1)
  return (g:gs)

generateNUniforms :: RNG -> Float -> Float -> Int -> IO [Float]
generateNUniforms _ _ _ 0 = return []
generateNUniforms rng a b n = do
  f <- liftM realToFrac $ getFlat rng (realToFrac a) (realToFrac b)
  fs <- generateNUniforms rng a b (n-1)
  return (f:fs)

generateGaussians :: RNG -> Float -> IO [Float]
generateGaussians rng sigma =
  (liftM2 (:)) (liftM realToFrac $ getGaussian rng (realToFrac sigma))
      (generateGaussians rng (realToFrac sigma))

-- corrupts the signal f with random values g
corruptSignalWithGaussian :: Float -> [(Float,Float)] -> [(Float,Float)]
corruptSignalWithGaussian sigma fs = zipWith addToY fs gs
  where
    gs = getGaussianVector sigma $ length fs
    addToY (x,y) g = (x,y+g)

corruptListWithGaussian :: Float -> [Float] -> [Float]
corruptListWithGaussian sigma fs = zipWith (+) fs gs
  where
    gs = getGaussianVector sigma $ length fs

corruptPairsWithGaussian :: Float -> [(Float,Float)] -> [(Float,Float)]
corruptPairsWithGaussian sigma ps = zipWith pairwiseAdd ps (zip gs1 gs2)
  where
    l = length ps
    gs = getGaussianVector sigma $ 2*l
    gs1 = take l gs
    gs2 = drop l gs
    pairwiseAdd (a,b) (c,d) = (a+c,b+d)

corruptWithGaussian :: RNG -> Float -> [Float] -> IO [Float]
corruptWithGaussian rng sigma (x:[]) = do
  x' <- addGaussian rng sigma x
  return [x]
corruptWithGaussian rng sigma (x:xs) = do
  x' <- addGaussian rng sigma x
  xs' <- corruptWithGaussian rng sigma xs
  return $ x' : xs'

getGaussianVector :: Float -> Int -> [Float]
getGaussianVector sigma n = unsafePerformIO $ do
  time <- getCurrentTime >>= return . utctDayTime
  rng <- newRNG mt19937
  setSeed rng $ (floor.(*1000000).toRational) time
  generateNGaussians rng sigma n

getGaussianVectorSeeded :: Word64 -> Float -> Int -> [Float]
getGaussianVectorSeeded seed sigma n = unsafePerformIO $ do
  rng <- newRNG mt19937
  setSeed rng seed
  generateNGaussians rng sigma n

getUniformVector :: Float -> Float -> Int -> [Float]
getUniformVector a b n = unsafePerformIO $ do
  time <- getCurrentTime >>= return . utctDayTime
  rng <- newRNG mt19937
  setSeed rng $ (floor.(*1000000).toRational) time
  generateNUniforms rng a b n

getUniformVectorSeeded :: Word64 -> Float -> Float -> Int -> [Float]
getUniformVectorSeeded seed a b n = unsafePerformIO $ do
  rng <- newRNG mt19937
  setSeed rng seed
  generateNUniforms rng a b n
