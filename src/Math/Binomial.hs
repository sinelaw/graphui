module Math.Binomial where

choose :: (Integral a) => a -> a -> a
choose n k = product [(n-k+1)..n] `div` product [1..k]
  
binomialCoefs :: (Integral a) => a -> [a]
binomialCoefs n = map (choose n) [0..n]

(.*.) :: (Num a) => [a] -> [a] -> [a]
(.*.) = zipWith (*)

binomialPoly :: (Integral a1, Num a) => a1 -> a -> a -> [a]
binomialPoly n a b = binCoefs .*. aPoly .*. bPoly 
    where binCoefs = map fromIntegral (binomialCoefs n)
          aPoly = map (a^) [0..(n-1)]
          bPoly = map (b^) [(n-1)..0]
