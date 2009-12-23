module Render where

import Data.Monoid
import qualified Data.IntMap as IntMap
import qualified Data.Set as Set

import qualified AnnotatedGraph as AG
import qualified Math.Vector2 as Vector2
import Math.Vector2((^/))
import Math.Bezier(bezierNSamples)
import qualified Graphics.DrawingCombinators as Draw

import qualified System.IO.Unsafe -- to hide the click collision detection


renderAG :: AG.AnnotatedGraph a b -> Draw.Draw AG.Ids
renderAG (AG.AG _ vrNodes vrEdges _) = mconcat (renderedNodes ++ renderedEdges) where 
    renderedNodes = renderElements vrNodes renderNode
    renderedEdges = renderElements vrEdges renderEdge
    renderElements elMap renderFunc = map (uncurry renderFunc) (IntMap.toList elMap)



scaleV :: RealFloat a => Vector2.Vector2 a -> Vector2.Vector2 a
scaleV = (^/ 500)

renderNode :: Int -> AG.VRDNode -> Draw.Draw AG.Ids
renderNode n vrdNode = Draw.translate (Vector2.getXY . coordsFromDOT . AG.positionN $ vrdNode) (nodeBox n)

onBoth :: (a -> b) -> (a,a) -> (b, b)
onBoth f (x,y) = (f x, f y)

renderEdge :: Int -> AG.VRDEdge -> Draw.Draw AG.Ids
renderEdge jd vrdEdge = mconcat (map mkLine (zip ps (tail ps))) `mappend` firstCircle  where
    ps = AG.bezierSamplesE vrdEdge
    --ps = AG.pointsE vrdEdge
    mkLine = fmap mkIds . uncurry Draw.line . onBoth (Vector2.getXY . coordsFromDOT)
    mkIds = const . Set.singleton $ AG.Id AG.Edge jd
    firstCircle = fmap mkIds 
                  . Draw.translate (Vector2.getXY . coordsFromDOT . last $ ps) 
                  . Draw.color (1,0,0,0.5) 
                  . Draw.scale 0.02 0.02 
                  $ Draw.circle



-- Temporary hacks
box :: Draw.Draw ()
box = Draw.scale 0.08 0.08
        $ Draw.color (1,0,0,1) 
        $ Draw.convexPoly
            [(1,1),(1,-1),(-1,-1),(-1,1)]

nodeBox :: Int -> Draw.Draw AG.Ids
nodeBox n = fmap (const . Set.singleton $ AG.Id AG.Node n) (Draw.color (c,1-c,1,0.5) box)
    where c = fromIntegral ((100*n) `mod` 256) / 256



resX :: Int
resX = 640 
resY :: Int
resY = 480 

coordsFromSDL :: Vector2.Vector2 Double -> Vector2.Vector2 Double
coordsFromSDL v = Vector2.vector2 (2*(x / fromIntegral resX) - 1) (1 - 2*(y / fromIntegral resY))
    where x = Vector2.getX v
          y = Vector2.getY v
  
coordsFromSDL' :: Double -> Double -> (Double, Double)
coordsFromSDL' x y = Vector2.getXY . coordsFromSDL $ (Vector2.vector2 x y)

coordsFromDOT :: Vector2.Vector2 Double -> Vector2.Vector2 Double
coordsFromDOT v = Vector2.vector2 (2*(x / fromIntegral resX) - 1) (2*(y / fromIntegral resY) - 1)
    where x = Vector2.getX v
          y = Vector2.getY v
  
locateClick :: (Integral a) => a -> a -> Draw.Draw AG.Ids -> Maybe AG.Ids
locateClick x y draw = System.IO.Unsafe.unsafePerformIO $ (getIds (fromIntegral x) (fromIntegral y) draw)
    where getIds x' y' draw' =  do
            let pos = (coordsFromSDL' x' y')
            res <- Draw.click pos draw'
            return res
