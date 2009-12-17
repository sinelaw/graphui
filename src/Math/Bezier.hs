module Math.Bezier where

import FRP.Yampa.Vector2 

-- Hitched from hps-0.1, Graphics.PS.Bezier --
bezier4 :: Vector2 a -> Vector2 a -> Vector2 a -> Vector2 a
           -> Double 
           -> Vector2 a
bezier4 (Vector2 x1 y1) (Vector2 x2 y2) (Vector2 x3 y3) (Vector2 x4 y4) mu =
    let a = 1 - mu
        b = a*a*a
        c = mu*mu*mu
        x = b*x1 + 3*mu*a*a*x2 + 3*mu*mu*a*x3 + c*x4
        y = b*y1 + 3*mu*a*a*y2 + 3*mu*mu*a*y3 + c*y4
    in (x,y)
--
       
       
bezierN :: [Vector2 a] -> Double -> Vector2 a
bezierN ps t = if (len < 4) then undefined else 
                 if (len > 4) then subBezier else (bezier4 (ps!!0) (ps!!1) (ps!!2) (ps!!3) t) where
                     subBezier = t *^ (bezierN (tail ps) t) ^+^ (1-t) *^ (bezierN (init ps) t)
                     len = length ps

bezierNSamples :: Int -> [Vector2 a] -> [Vector2 a]
bezierNSamples 0 _  = []
bezierNSamples 1 ps = [bezierN ps 0]
bezierNSamples n ps = map (bezierN ps) (take n [0, 1/(fromIntegral n - 1)..])