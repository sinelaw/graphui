module Render where

import Data.Monoid
import qualified Data.Map as Map

import qualified AnnotatedGraph

import qualified Graphics.DrawingCombinators as Draw
import Math.Bezier(bezierN)

renderAG :: AnnotatedGraph.AnnotatedGraph -> Draw.Draw AnnotatedGraph.Id
renderAG (AnnotatedGraph.AG g vrNodes vrEdges) = mconcat (renderedNodes ++ renderedEdges) where 
    renderedNodes = renderElements vrNodes renderNode
    renderedEdges = renderElements vrEdges renderEdge
    renderElements elMap renderFunc = map (uncurry renderFunc) (Map.toList elMap)


renderNode :: Int -> AnnotatedGraph.VRDNode -> Draw.Draw AnnotatedGraph.Id
renderNode jd vrdNode = Draw.empty 

renderEdge :: Int -> AnnotatedGraph.VRDEdge -> Draw.Draw AnnotatedGraph.Id
renderEdge jd vrdEdge = mconcat (map mkLine (zip ps (tail ps))) where
    ps = AnnotatedGraph.pointsE vrdEdge
    mkLine = (fmap mkId) . (uncurry Draw.line)
    mkId = const (AnnotatedGraph.Id [(AnnotatedGraph.Edge, jd)])
