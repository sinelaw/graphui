module Layout where

import qualified GraphVizUtils

import qualified Data.Graph.Inductive as Graph

import qualified AnnotatedGraph as AG
import qualified Math.Vector2 as Vector2

import qualified Data.GraphViz.Attributes.Complete as GVAttrs
import qualified Data.GraphViz.Types.Generalised as GVG
import qualified Data.GraphViz.Types as GVTypes

import qualified Data.IntMap as IntMap

pointToVec :: (Fractional a) => GVAttrs.Point -> Vector2.Vector2 a
pointToVec (GVAttrs.Point x y _ _) = Vector2.vector2 (realToFrac x) (realToFrac y)

fromInch :: (Num a) => a -> a
fromInch = (*72) -- see dot documentation

addVRDNAttr :: (Fractional a) => GVAttrs.Attribute -> AG.VRDNode a -> AG.VRDNode a
addVRDNAttr attr vrdn = case attr of
  GVAttrs.Pos (GVAttrs.PointPos p) -> vrdn{AG.positionN = (pointToVec p)}
  GVAttrs.Width w -> vrdn{AG.widthN = realToFrac . fromInch $ w}
  GVAttrs.Height h -> vrdn{AG.heightN = realToFrac . fromInch $ h}
  _ -> vrdn

splineToVecs :: (Fractional a) => GVAttrs.Spline -> [Vector2.Vector2 a]
splineToVecs spline = map pointToVec points
    where fixP (Just p) _ = p
          fixP Nothing p' = p'
          points = [fixP s (head ps)] ++ ps ++ [fixP e (last ps)]
          s = GVAttrs.startPoint spline
          e = GVAttrs.endPoint spline
          ps = GVAttrs.splinePoints spline
          
setPointsE :: (RealFloat a) => [Vector2.Vector2 a] -> AG.VRDEdge a -> AG.VRDEdge a
setPointsE points vrde = vrde{AG.pointsE = points, 
                              AG.bezierSamplesNumE = newNumSamples
                             }
                         where newNumSamples = length points * 2 -- todo fix this magic number
                         
addVRDEAttr :: (RealFloat a) => GVAttrs.Attribute -> AG.VRDEdge a -> AG.VRDEdge a
addVRDEAttr (GVAttrs.Pos (GVAttrs.SplinePos (s:_))) = setPointsE . splineToVecs $ s
addVRDEAttr _ = id
  
nodeAttrsToVRDN :: (Fractional a) => GVAttrs.Attributes -> AG.VRDNode a -> AG.VRDNode a
nodeAttrsToVRDN attrs vrdn = foldr addVRDNAttr vrdn attrs

edgeAttrsToVRDE :: (RealFloat a) => GVAttrs.Attributes -> AG.VRDEdge a -> AG.VRDEdge a
edgeAttrsToVRDE attrs vrde = foldr addVRDEAttr vrde attrs


addVRGAttr :: (Ord a, Fractional a) => GVAttrs.Attribute -> AG.VRGraph a -> AG.VRGraph a
addVRGAttr (GVAttrs.BoundingBox (GVAttrs.Rect _ p2)) vrg = vrg{AG.widthG = mdim,
                                                      AG.heightG = mdim}
    where v = pointToVec p2
          mdim = max (Vector2.getX v) (Vector2.getY v)
addVRGAttr _ vrg = vrg

-- todo use graphToGraph, to utilize the manually-set attribute information
autoLayout :: (RealFloat c) => AG.AnnotatedGraph a b c -> AG.AnnotatedGraph a b c
autoLayout ag = AG.AG gr (AG.VRNode newVRN) (AG.VREdge newVRE) newVRG
  where gr = AG.graph ag
        oldVRN = AG.unVRNode $ AG.vrNodes ag
        oldVRE = AG.unVREdge $ AG.vrEdges ag
        oldVRG = AG.vrGraph ag
        
        (grDotAttrs, dotizedGr) = GraphVizUtils.dotizedGraph'' gr True
        newVRE = foldr convToVRE (AG.unVREdge AG.vrEdgeEmpty) (Graph.labEdges dotizedGr)
        newVRN = Graph.ufold convToVRN (AG.unVRNode AG.vrNodeEmpty) dotizedGr
        newVRG = convToVRG grDotAttrs oldVRG{AG.needsLayout = False} 
        
        convToVRN (_, id', (gvAttrs, _), _)  = IntMap.insert id' (nodeAttrsToVRDN gvAttrs vrdn)
            where vrdn = IntMap.findWithDefault AG.defaultVRDN id' oldVRN
        convToVRE (_,_, (gvAttrs, (id', _))) = IntMap.insert id' (edgeAttrsToVRDE gvAttrs vrde)
            where vrde = IntMap.findWithDefault AG.defaultVRDE id' oldVRE

        
--        convToVRG :: [GVTypes.GlobalAttributes] -> AG.VRGraph   -> AG.VRGraph
        convToVRG ((GVG.GraphAttrs gattrs):attrs) vrg = convToVRG attrs (foldr addVRGAttr vrg gattrs)
        convToVRG (_:attrs) vrg = convToVRG attrs vrg
        convToVRG []        vrg = vrg

layoutIfNeeded :: (RealFloat c) => AG.AnnotatedGraph a b c -> AG.AnnotatedGraph a b c
layoutIfNeeded ag = if (AG.needsLayout . AG.vrGraph $ ag) then autoLayout ag else ag

