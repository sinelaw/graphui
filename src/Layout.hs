module Layout where

import qualified GraphVizUtils

import qualified Data.Graph.Inductive as Graph

import qualified AnnotatedGraph as AG
import qualified Math.Vector2 as Vector2
import qualified Math.Bezier as Bezier

import qualified Data.GraphViz.Attributes as GVAttrs
import qualified Data.GraphViz.Types as GVTypes

import qualified Data.IntMap as IntMap


pointToVec :: GVAttrs.Point -> Vector2.Vector2 Double 
pointToVec (GVAttrs.Point x y) = Vector2.vector2 (fromIntegral x) (fromIntegral y)
pointToVec (GVAttrs.PointD x y) = Vector2.vector2 x y

fromInch :: (Fractional a) => a -> a
fromInch x = x * 72 -- see dot documentation

addVRDNAttr :: GVAttrs.Attribute -> AG.VRDNode -> AG.VRDNode
addVRDNAttr attr vrdn = case attr of
  GVAttrs.Pos (GVAttrs.PointPos p) -> vrdn{AG.positionN = (pointToVec p)}
  GVAttrs.Width w -> vrdn{AG.widthN = fromInch w}
  GVAttrs.Height h -> vrdn{AG.heightN = fromInch h}
  _ -> vrdn

splineToVecs :: GVAttrs.Spline -> [Vector2.Vector2 Double]
splineToVecs (GVAttrs.Spline s e ps) = map pointToVec points
    where fixP (Just p) _ = p
          fixP Nothing p' = p'
          points = [fixP s (head ps)] ++ ps ++ [fixP e (last ps)]
          
setPointsE :: [Vector2.Vector2 Double] -> AG.VRDEdge -> AG.VRDEdge
setPointsE points vrde = vrde{AG.pointsE = points, 
                              AG.bezierSamplesNumE = newNumSamples,
                              AG.bezierSamplesE = Bezier.bezierNSamples newNumSamples points
                             }
                         where newNumSamples = (length points)*2 -- todo fix this magic number
                         
addVRDEAttr :: GVAttrs.Attribute -> AG.VRDEdge -> AG.VRDEdge
addVRDEAttr (GVAttrs.Pos (GVAttrs.SplinePos (s:_))) = setPointsE . splineToVecs $ s
addVRDEAttr _ = id
  
nodeAttrsToVRDN :: GVAttrs.Attributes -> AG.VRDNode -> AG.VRDNode
nodeAttrsToVRDN attrs vrdn = foldr addVRDNAttr vrdn attrs

edgeAttrsToVRDE :: GVAttrs.Attributes -> AG.VRDEdge -> AG.VRDEdge
edgeAttrsToVRDE attrs vrde = foldr addVRDEAttr vrde attrs


addVRGAttr :: GVAttrs.Attribute -> AG.VRGraph -> AG.VRGraph
addVRGAttr (GVAttrs.Bb (GVAttrs.Rect _ p2)) vrg = vrg{AG.widthG = Vector2.getX v,
                                                      AG.heightG = Vector2.getY v}
    where v = pointToVec p2
addVRGAttr _ vrg = vrg

-- todo use graphToGraph, to utilize the manually-set attribute information
autoLayout :: AG.AnnotatedGraph a b -> AG.AnnotatedGraph a b
autoLayout ag = AG.AG gr newVRN newVRE newVRG
  where gr = AG.graph ag
        oldVRN = AG.vrNodes ag
        oldVRE = AG.vrEdges ag
        oldVRG = AG.vrGraph ag
        
        (grDotAttrs, dotizedGr) = GraphVizUtils.dotizedGraph'' True gr
        newVRE = foldr convToVRE AG.vrEdgeEmpty (Graph.labEdges dotizedGr)
        newVRN = Graph.ufold convToVRN AG.vrNodeEmpty dotizedGr
        newVRG = convToVRG grDotAttrs oldVRG{AG.needsLayout = False} 
        
        convToVRN (_, id', (gvAttrs, _), _)  = IntMap.insert id' (nodeAttrsToVRDN gvAttrs vrdn)
            where vrdn = IntMap.findWithDefault AG.defaultVRDN id' oldVRN
        convToVRE (_,_, (gvAttrs, (id', _))) = IntMap.insert id' (edgeAttrsToVRDE gvAttrs vrde)
            where vrde = IntMap.findWithDefault AG.defaultVRDE id' oldVRE

        
        convToVRG :: [GVTypes.GlobalAttributes] -> AG.VRGraph -> AG.VRGraph
        convToVRG ((GVTypes.GraphAttrs gattrs):attrs) vrg = convToVRG attrs (foldr addVRGAttr vrg gattrs)
        convToVRG (_:attrs) vrg = convToVRG attrs vrg
        convToVRG []        vrg = vrg



