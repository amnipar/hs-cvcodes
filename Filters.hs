module Filters
( convolve1D
) where

-- | Calculates naively the 1D convolution of two signals g and f.
convolve1D :: [Float] -> [(Float,Float)] -> [(Float,Float)]
convolve1D g f = zip xs $ take (length xs) gy
  where
    xs = map fst f
    ys = map snd f
    -- need to pad the signal from both ends to make the kernel fit
    ys' = (replicate c (head ys)) ++ ys ++ (replicate c (last ys))
    gy = map (sum . zipWith ( * ) (reverse g)) (init $ tails ys')
    l = length g
    c = l `div` 2

g :: Float -> Int -> Float
g s x = (1 / (sqrt $ 2 * pi * s**2)) * exp (-((fi x)**2)/(2 * s**2))

kernel :: (Int -> Float) -> Int -> [Float]
kernel f r = [f x | x <- [-r..r]]

createMask :: (Float -> Int -> Float) -> Int -> [Float]
createMask f l = kernel (f s) r
  where
    r | even l = (l `div` 2) - 1
      | otherwise = (l `div` 2)
    s = (fi l) / 6
