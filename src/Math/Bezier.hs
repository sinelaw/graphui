module Math.Bezier where

import Math.Vector2
import Math.Binomial

  
-- Hitched from hps-0.1, Graphics.PS.Bezier --
bezier4 :: (RealFloat a) => a 
        -> Vector2 a -> Vector2 a -> Vector2 a -> Vector2 a
        -> Vector2 a
bezier4 mu v1 v2 v3 v4 = foldr (^+^) zeroVector (zipWith (*^) (binomialPoly 3 (1-mu) mu) [v1,v2,v3,v4])
--
       
       
bezierN :: (RealFloat a) => a -> [Vector2 a] -> Vector2 a
bezierN t ps@(p0:p1:p2:p3:[])  = bezier4 t p0 p1 p2 p3 
bezierN t ps@(p0:p1:p2:p3:_)   = t *^ bezierN t (tail ps)  ^+^  (1-t) *^ bezierN t (init ps)
bezierN _ _ = error "Need at least 4 points to calculate bezier"


bezierNSamples :: (RealFloat a) => Int -> [Vector2 a] -> [Vector2 a]
bezierNSamples 0 _  = []
bezierNSamples 1 ps = [bezierN 0 ps]
bezierNSamples n ps = map (`bezierN` ps) (take n uniformSamples)
    where uniformSamples = iterate (+1/(fromIntegral n - 1)) 0