module Render where

import Data.Monoid
import qualified Data.IntMap as IntMap
import qualified Data.Set as Set

import qualified AnnotatedGraph as AG
import qualified Math.Vector2 as Vector2
import qualified Graphics.DrawingCombinators as Draw

import System.IO.Unsafe(unsafePerformIO) -- to hide the click collision detection


renderAG :: AG.AnnotatedGraph a b -> Draw.Draw AG.Ids
renderAG (AG.AG _ vrNodes vrEdges vrGraph) = Draw.scale s s drawAG
    where drawAG = mconcat (renderedNodes ++ renderedEdges) 
          renderedNodes = renderElements vrNodes (renderNode vrGraph)
          renderedEdges = renderElements vrEdges (renderEdge vrGraph)
          renderElements elMap renderFunc = map (uncurry renderFunc) (IntMap.toList elMap)
          s = AG.zoomG vrGraph



renderNode :: AG.VRGraph -> Int -> AG.VRDNode -> Draw.Draw AG.Ids
renderNode vrg id' vrdNode = Draw.translate (Vector2.getXY . coordsFromDOT gw gh . AG.positionN $ vrdNode) (nodeBox w h id')
    where gw = AG.widthG vrg
          gh = AG.heightG vrg
          w = AG.widthN vrdNode / gw
          h = AG.heightN vrdNode / gh

onBoth :: (a -> b) -> (a,a) -> (b, b)
onBoth f (x,y) = (f x, f y)

renderEdge :: AG.VRGraph -> Int -> AG.VRDEdge -> Draw.Draw AG.Ids
renderEdge vrg id' vrdEdge = mconcat (map mkLine (zip ps (tail ps))) `mappend` firstCircle  
  where ps = AG.bezierSamplesE vrdEdge
        --ps = AG.pointsE vrdEdge
        mkLine = fmap mkIds . uncurry Draw.line . onBoth (Vector2.getXY . coordsFromDOT w h)
        mkIds = const . Set.singleton $ AG.Id AG.Edge id'
        firstCircle = fmap mkIds 
                      . Draw.translate (Vector2.getXY . coordsFromDOT w h . last $ ps) 
                      . Draw.color (1,0,0,0.5) 
                      . Draw.scale 0.02 0.02 
                      $ Draw.circle

        w = AG.widthG vrg
        h = AG.heightG vrg
                      



-- Temporary hacks
square :: Draw.Draw ()
square = Draw.convexPoly [(1,1),(1,-1),(-1,-1),(-1,1)]

nodeBox :: Double -> Double -> Int -> Draw.Draw AG.Ids
nodeBox w h n = fmap (const . Set.singleton $ AG.Id AG.Node n) (Draw.color (c,1-c,1,0.5) . Draw.scale w h $ Draw.circle)
    where c = fromIntegral ((100*n) `mod` 256) / 256



coordsFromSDL :: (Fractional a) => a -> a -> Vector2.Vector2 a -> Vector2.Vector2 a
coordsFromSDL w h v = Vector2.vector2 (2*(x / w) - 1) (1 - 2*(y / h))
    where x = Vector2.getX v
          y = Vector2.getY v
  
coordsFromSDL' :: (Fractional a) => a -> a -> a -> a -> (a, a)
coordsFromSDL' w h x y = Vector2.getXY . coordsFromSDL w h $ (Vector2.vector2 x y)

coordsFromDOT :: (Fractional a) => a -> a -> Vector2.Vector2 a -> Vector2.Vector2 a
coordsFromDOT w h v = Vector2.vector2 (2*(x / w) - 1) (2*(y / h) - 1)
    where x = Vector2.getX v
          y = Vector2.getY v
  
locateClick :: (Integral a, Integral b) => Double -> Double -> a -> b -> Draw.Draw c -> Maybe c
locateClick w h x y draw = unsafePerformIO $ (getIds (fromIntegral x) (fromIntegral y) draw)
    where getIds x' y' draw' =  do
            let pos = (coordsFromSDL' w h x' y')
            res <- Draw.click pos draw'
            return res
