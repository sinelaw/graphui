module Layout where

import qualified Data.Graph.Inductive as Graph

import qualified AnnotatedGraph as AG
import qualified Math.Vector2 as Vector2

import qualified Data.GraphViz as GraphViz
import qualified Data.GraphViz.Attributes as GVAttrs

import qualified Data.IntMap as IntMap

-- fclabel stuff
import Prelude hiding ((.),id,mod)
-- import Control.Category
import Data.Record.Label
--

pointToVec :: GVAttrs.Point -> Vector2.Vector2 Double 
pointToVec (GVAttrs.Point x y) = Vector2.vector2 (fromIntegral x) (fromIntegral y)
pointToVec (GVAttrs.PointD x y) = Vector2.vector2 x y

addVRDNAttr :: GVAttrs.Attribute -> AG.VRDNode -> AG.VRDNode
addVRDNAttr attr vrdn = case attr of
  GVAttrs.Pos (GVAttrs.PointPos p) -> vrdn{AG.positionN = (pointToVec p)}
  GVAttrs.Width w -> vrdn{AG.widthN = w}
  GVAttrs.Height h -> vrdn{AG.heightN = h}
  _ -> vrdn
  
addVRDEAttr :: GVAttrs.Attribute -> AG.VRDEdge -> AG.VRDEdge
addVRDEAttr attr vrde = case attr of
  GVAttrs.Pos (GVAttrs.SplinePos ((GVAttrs.Spline s e ps):_)) -> vrde{AG.pointsE = vecs, AG.widthE=3} -- =3 is a debug thing
      where fixP (Just p) _ = p
            fixP Nothing p' = p'
            points = [fixP s (head ps)] ++ ps ++ [fixP e (last ps)]
            vecs = map pointToVec points
  _ -> vrde
  
nodeAttrsToVRDN :: GVAttrs.Attributes -> AG.VRDNode
nodeAttrsToVRDN = foldr addVRDNAttr AG.defaultVRDN

edgeAttrsToVRDE :: GVAttrs.Attributes -> AG.VRDEdge
edgeAttrsToVRDE = foldr addVRDEAttr AG.defaultVRDE

autoLayout :: AG.AnnotatedGraph a b -> AG.AnnotatedGraph a b
autoLayout ag = AG.AG gr newVRN newVRE newVRG
  where gr = AG.graph ag
        -- oldVRN = AG.vrNodes ag
        oldVRE = AG.vrEdges ag
        newVRE = foldr convToVRE AG.vrEdgeEmpty (Graph.labEdges dotizedGr)
        dotizedGr = GraphViz.dotizeGraph True gr
        newVRN = Graph.ufold convToVRN AG.vrNodeEmpty dotizedGr
        newVRG = set AG.lNeedsLayout False (AG.vrGraph ag)
        convToVRN (_, node, (grvAttrs, _), _) = IntMap.insert node (nodeAttrsToVRDN grvAttrs)
        convToVRE (_,_, (gvAttrs, (id', _))) = IntMap.insert id' (edgeAttrsToVRDE gvAttrs)

