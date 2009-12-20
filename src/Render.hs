module Render where

import Data.Monoid
import qualified Data.IntMap as IntMap

import qualified Data.Set as Set

import qualified AnnotatedGraph
import qualified Math.Vector2 as Vector2
import Math.Bezier(bezierNSamples)

import qualified Graphics.DrawingCombinators as Draw

renderAG :: AnnotatedGraph.AnnotatedGraph a b -> Draw.Draw AnnotatedGraph.Id
renderAG (AnnotatedGraph.AG g vrNodes vrEdges) = mconcat (renderedNodes ++ renderedEdges) where 
    renderedNodes = renderElements vrNodes renderNode
    renderedEdges = renderElements vrEdges renderEdge
    renderElements elMap renderFunc = map (uncurry renderFunc) (IntMap.toList elMap)



renderNode :: Int -> AnnotatedGraph.VRDNode -> Draw.Draw AnnotatedGraph.Id
renderNode jd vrdNode = draw1

onBoth :: (a -> b) -> (a,a) -> (b, b)
onBoth f (x,y) = (f x, f y)

renderEdge :: Int -> AnnotatedGraph.VRDEdge -> Draw.Draw AnnotatedGraph.Id
renderEdge jd vrdEdge = mconcat (map mkLine (zip ps (tail ps))) where
    ps = bezierNSamples (AnnotatedGraph.bezierSamplesE vrdEdge) (AnnotatedGraph.pointsE vrdEdge)
    mkLine = (fmap mkId) . (uncurry Draw.line) . (onBoth Vector2.vector2XY)
    mkId = const (AnnotatedGraph.Id (Set.singleton (AnnotatedGraph.Edge, jd)))



-- Temporary hacks
box :: Draw.Draw ()
box = Draw.scale 0.3 0.3 
        $ Draw.color (1,0,0,1) 
        $ Draw.convexPoly
            [(1,1),(1,-1),(-1,-1),(-1,1)]

draw1 :: Draw.Draw AnnotatedGraph.Id
draw1 = fmap (const (AnnotatedGraph.newId AnnotatedGraph.Node 1)) (Draw.color (0,0,1,0.5) box)
