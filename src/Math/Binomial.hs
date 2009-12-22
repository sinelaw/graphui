module Math.Binomial where

factorial :: (Num t, Enum t) => t -> t
factorial n = product [1..n]

choose :: (Ord a, Fractional a, Enum a) => a -> a -> a
choose n k = if (k > n) 
             then 0
             else (factorial n) / ((factorial k) * (factorial (n-k)))
  
binomialCoefs :: (Ord a, Fractional a, Enum a) => a -> [a]
binomialCoefs n = map (choose n) [0..n]

binomialPoly :: (Integral b, Fractional b, Num a) => b -> a -> a -> [a]
binomialPoly n a b = zipWith (*) (zipWith (*) binCoefs aPoly) bPoly
    where binCoefs = map fromIntegral (binomialCoefs n)
          aPoly = map (a^) [0..(n-1)]
          bPoly = map (b^) [n-1..0]
