module Random
( addGaussian
, generateNGaussians
, generateGaussians
, corruptGaussian
, corruptWithGaussian
, newRNG
, setSeed
, mt19937
, getGaussian
, getGaussianVector
) where

import Control.Monad
import GSL.Random.Gen
import GSL.Random.Dist
import System.IO.Unsafe

addGaussian :: RNG -> Double -> Double -> IO Double
addGaussian rng sigma x = do
  g <- getGaussian rng sigma
  return $ x + g

generateNGaussians :: RNG -> Double -> Int -> IO [Double]
generateNGaussians _ _ 0 = return []
generateNGaussians rng sigma n = do
  g <- getGaussian rng sigma
  gs <- generateNGaussians rng sigma (n-1)
  return (g:gs)

generateGaussians :: RNG -> Double -> IO [Double]
generateGaussians rng sigma =
  (liftM2 (:)) (getGaussian rng sigma)
    (generateGaussians rng sigma)

-- corrupts the signal f with random values g
corruptGaussian f g = zipWith addToY f g
  where
    addToY (x,y) g = (x,y+g)

corruptWithGaussian :: RNG -> Double -> [Double] -> IO [Double]
corruptWithGaussian rng sigma (x:[]) = do
  x' <- addGaussian rng sigma x
  return [x]
corruptWithGaussian rng sigma (x:xs) = do
  x' <- addGaussian rng sigma x
  xs' <- corruptWithGaussian rng sigma xs
  return $ x' : xs'

getGaussianVector seed sigma n = unsafePerformIO $ do
  rng <- newRNG mt19937
  setSeed rng seed
  generateNGaussians rng sigma n
