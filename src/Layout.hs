module Layout where

import qualified Data.Graph.Inductive as Graph

import qualified AnnotatedGraph as AG
import qualified Math.Vector2 as Vector2

import qualified Data.GraphViz as GraphViz
import qualified Data.GraphViz.Attributes as GVAttrs

import qualified Data.IntMap as IntMap

-- Debugging
import Debug.Trace
traceIt :: (Show a) => String -> a -> a
traceIt p x = trace (p ++ " " ++ (show x)) x
---

pointToVec :: GVAttrs.Point -> Vector2.Vector2 Double 
pointToVec (GVAttrs.Point x y) = Vector2.vector2 (fromIntegral x) (fromIntegral y)
pointToVec (GVAttrs.PointD x y) = Vector2.vector2 x y

addVRDNAttr :: GVAttrs.Attribute -> AG.VRDNode -> AG.VRDNode
addVRDNAttr attr vrdn = case attr of
  GVAttrs.Pos (GVAttrs.PointPos p) -> vrdn{AG.positionN = (pointToVec p)}
  GVAttrs.Width w -> vrdn{AG.widthN = w}
  GVAttrs.Height h -> vrdn{AG.heightN = h}
  _ -> vrdn

splineToVecs :: GVAttrs.Spline -> [Vector2.Vector2 Double]
splineToVecs (GVAttrs.Spline s e ps) = map pointToVec points
    where fixP (Just p) _ = p
          fixP Nothing p' = p'
          points = [fixP s (head ps)] ++ ps ++ [fixP e (last ps)]
          
setPointsE :: [Vector2.Vector2 Double] -> AG.VRDEdge -> AG.VRDEdge
setPointsE points vrde = vrde{AG.pointsE = points, 
                              AG.widthE=3} -- =3 is a debug thing
                         
addVRDEAttr :: GVAttrs.Attribute -> AG.VRDEdge -> AG.VRDEdge
addVRDEAttr (GVAttrs.Pos (GVAttrs.SplinePos (s:_))) = setPointsE . splineToVecs $ s
addVRDEAttr attr = trace (show attr) id
  
nodeAttrsToVRDN :: GVAttrs.Attributes -> AG.VRDNode
nodeAttrsToVRDN = foldr addVRDNAttr AG.defaultVRDN

edgeAttrsToVRDE :: GVAttrs.Attributes -> AG.VRDEdge
edgeAttrsToVRDE = foldr addVRDEAttr AG.defaultVRDE

-- todo use graphToGraph, to utilize the manually-set attribute information
autoLayout :: AG.AnnotatedGraph a b -> AG.AnnotatedGraph a b
autoLayout ag = AG.AG gr newVRN newVRE newVRG
  where gr = AG.graph ag
        -- oldVRN = AG.vrNodes ag
        -- oldVRE = AG.vrEdges ag
        dotizedGr = GraphViz.dotizeGraph True gr
        newVRE = foldr convToVRE AG.vrEdgeEmpty (Graph.labEdges dotizedGr)
        newVRN = Graph.ufold convToVRN AG.vrNodeEmpty dotizedGr
        newVRG = (AG.vrGraph ag){AG.needsLayout = False}
--        newVRG = foldr addVRDGAttr (AG.vrGraph
        convToVRN (_, id', (gvAttrs, _), _)  = IntMap.insert id' (nodeAttrsToVRDN gvAttrs)
        convToVRE (_,_, (gvAttrs, (id', _))) = IntMap.insert id' (edgeAttrsToVRDE (traceIt "gvAttrs:" gvAttrs))

