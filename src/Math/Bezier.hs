module Math.Bezier where

import Math.Vector2

-- Hitched from hps-0.1, Graphics.PS.Bezier --
bezier4 :: (RealFloat a) => Vector2 a -> Vector2 a -> Vector2 a -> Vector2 a
        -> a
        -> Vector2 a
bezier4 v1 v2 v3 v4 mu = (a^3)*^v1 ^+^ (3*mu*a^2)*^v2 ^+^ (3*mu^2*a)*^v3 ^+^ (mu^3)*^v4
    where a = 1 - mu
--
       
       
bezierN :: (RealFloat a) => [Vector2 a] -> a -> Vector2 a
bezierN ps@(p0:p1:p2:p3:[]) t = bezier4 p0 p1 p2 p3 t
bezierN ps@(p0:p1:p2:p3:_)  t = t *^ bezierN (tail ps) t  ^+^  (1-t) *^ bezierN (init ps) t
bezierN _ _ = error "Need at least 4 points to calculate bezier"


bezierNSamples :: (RealFloat a) => Int -> [Vector2 a] -> [Vector2 a]
bezierNSamples 0 _  = []
bezierNSamples 1 ps = [bezierN ps 0]
bezierNSamples n ps = map (bezierN ps) (take n uniformSamples)
    where uniformSamples = iterate (+1/(fromIntegral n - 1)) 0