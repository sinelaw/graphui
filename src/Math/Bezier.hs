module Math.Bezier where

-- Hitched from hps-0.1, Graphics.PS.Bezier --
bezier4 :: (Double, Double) -> (Double, Double) -> (Double, Double) -> (Double, Double) -> Double -> (Double, Double)
bezier4 (x1, y1) (x2, y2) (x3, y3) (x4, y4) mu =
    let a = 1 - mu
        b = a*a*a
        c = mu*mu*mu
        x = b*x1 + 3*mu*a*a*x2 + 3*mu*mu*a*x3 + c*x4
        y = b*y1 + 3*mu*a*a*y2 + 3*mu*mu*a*y3 + c*y4
    in (x,y)
--
       
       
infixl 0 `v2Add`
v2Add :: (Num t, Num u) => (t, u) -> (t, u) -> (t, u)
(x,y) `v2Add` (x',y') = (x+x', y+y')

infixl 1 `v2SMul`
v2SMul :: (Num a) => a -> (a, a) -> (a, a)
s `v2SMul` (x,y) = (s*x, s*y)

bezierN :: [(Double, Double)] -> Double -> (Double, Double)
bezierN ps t = if (len < 4) then undefined else 
                 if (len > 4) then subBezier else (bezier4 (ps!!0) (ps!!1) (ps!!2) (ps!!3) t) where
                     subBezier = t `v2SMul` (bezierN (tail ps) t) `v2Add` (1-t) `v2SMul` (bezierN (init ps) t)
                     len = length ps

bezierNSamples :: Int -> [(Double, Double)] -> [(Double, Double)]
bezierNSamples 0 _  = []
bezierNSamples 1 ps = [bezierN ps 0]
bezierNSamples n ps = map (bezierN ps) (take n [0, 1/(fromIntegral n - 1)..])