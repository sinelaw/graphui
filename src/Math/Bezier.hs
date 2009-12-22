module Math.Bezier where

import Math.Vector2

-- Hitched from hps-0.1, Graphics.PS.Bezier --
bezier4 :: (RealFloat a) => a 
        -> Vector2 a -> Vector2 a -> Vector2 a -> Vector2 a
        -> Vector2 a
bezier4 mu v1 v2 v3 v4 = (a^3)*^v1 
                         ^+^ (3*mu*a^2)*^v2 
                         ^+^ (3*mu^2*a)*^v3 
                         ^+^ (mu^3)*^v4
    where a = 1 - mu
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