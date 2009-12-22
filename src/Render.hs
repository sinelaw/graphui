module Render where

import Data.Monoid
import qualified Data.IntMap as IntMap
import qualified Data.Set as Set

import qualified AnnotatedGraph as AG
import qualified Math.Vector2 as Vector2
import Math.Bezier(bezierNSamples)

import qualified Graphics.DrawingCombinators as Draw

renderAG :: AG.AnnotatedGraph a b -> Draw.Draw AG.Ids
renderAG (AG.AG _ vrNodes vrEdges _) = mconcat (renderedNodes ++ renderedEdges) where 
    renderedNodes = renderElements vrNodes renderNode
    renderedEdges = renderElements vrEdges renderEdge
    renderElements elMap renderFunc = map (uncurry renderFunc) (IntMap.toList elMap)



renderNode :: Int -> AG.VRDNode -> Draw.Draw AG.Ids
renderNode n vrdNode = Draw.translate (Vector2.vector2XY . (Vector2.^/ 1000) . AG.positionN $ vrdNode) (nodeBox n)

onBoth :: (a -> b) -> (a,a) -> (b, b)
onBoth f (x,y) = (f x, f y)

renderEdge :: Int -> AG.VRDEdge -> Draw.Draw AG.Ids
renderEdge jd vrdEdge = mconcat (map mkLine (zip ps (tail ps))) where
    ps = bezierNSamples (AG.bezierSamplesE vrdEdge) (AG.pointsE vrdEdge)
    mkLine = fmap mkIds . uncurry Draw.line . onBoth Vector2.vector2XY
    mkIds = const . Set.singleton $ AG.Id AG.Edge jd



-- Temporary hacks
box :: Draw.Draw ()
box = Draw.scale 0.02 0.02
        $ Draw.color (1,0,0,1) 
        $ Draw.convexPoly
            [(1,1),(1,-1),(-1,-1),(-1,1)]

nodeBox :: Int -> Draw.Draw AG.Ids
nodeBox n = fmap (const . Set.singleton $ AG.Id AG.Node n) (Draw.color (0,0,1,0.5) box)
