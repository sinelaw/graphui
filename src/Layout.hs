module Layout where

import qualified Data.Graph.Inductive as Graph

import qualified AnnotatedGraph as AG
import qualified Math.Vector2 as Vector2

import qualified Data.GraphViz as GraphViz
import qualified Data.GraphViz.Attributes as GVAttrs

import qualified Data.IntMap as IntMap
import qualified Data.Set as Set

pointToVec :: GVAttrs.Point -> Vector2.Vector2 Double 
pointToVec (GVAttrs.Point x y) = Vector2.vector2 (fromIntegral x) (fromIntegral y)
pointToVec (GVAttrs.PointD x y) = Vector2.vector2 x y

addVRDNAttr :: GVAttrs.Attribute -> AG.VRDNode -> AG.VRDNode
addVRDNAttr attr vrdn = case attr of
  GVAttrs.Pos (GVAttrs.PointPos p) -> vrdn{AG.positionN = (pointToVec p)}
  GVAttrs.Width w -> vrdn{AG.widthN = w}
  GVAttrs.Height h -> vrdn{AG.heightN = h}
  _ -> vrdn
  
nodeAttrsToVRDN :: GVAttrs.Attributes -> AG.VRDNode
nodeAttrsToVRDN = foldr addVRDNAttr AG.defaultVRDN

autoLayout :: AG.AnnotatedGraph a b -> AG.AnnotatedGraph a b
autoLayout ag = AG.AG gr newVRN newVRE newVRG
  where gr = AG.graph ag
        oldVRN = AG.vrNodes ag
        oldVRE = AG.vrEdges ag
        newVRE = oldVRE
        dotizedGr = GraphViz.dotizeGraph True gr
        newVRN = Graph.ufold convToVRN AG.vrNodeEmpty dotizedGr
        newVRG = AG.vrGraph ag
        convToVRN (inEdges, node, (grvAttrs, label), outEdges) vrNodes = 
            IntMap.insert node (nodeAttrsToVRDN grvAttrs) vrNodes

