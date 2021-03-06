module Render where

import Data.Monoid
import qualified Data.IntMap as IntMap
import qualified Data.Set as Set

import qualified AnnotatedGraph as AG
import qualified Math.Vector2 as Vector2
import qualified Graphics.DrawingCombinators as Draw


renderAG :: AG.AnnotatedGraph a b Draw.R -> Draw.Image (Maybe AG.Ids)
renderAG (AG.AG _ vrNodes vrEdges vrGraph) = Draw.scale s s Draw.%% drawAG
    where drawAG = mconcat (renderedNodes ++ renderedEdges) 
          renderedNodes = renderElements (AG.unVRNode vrNodes) (renderNode vrGraph)
          renderedEdges = renderElements (AG.unVREdge vrEdges) (renderEdge vrGraph)
          renderElements elMap renderFunc = map (uncurry renderFunc) (IntMap.toList elMap)
          s = AG.zoomG vrGraph



renderNode :: AG.VRGraph Draw.R -> Int -> AG.VRDNode Draw.R -> Draw.Image (Maybe AG.Ids)
renderNode vrg id' vrdNode = Draw.translate transxy Draw.%% nodeBox w h nodeId hovered selected
    where gw = AG.widthG vrg
          gh = AG.heightG vrg
          w = AG.widthN vrdNode / gw
          h = AG.heightN vrdNode / gh
          transxy = Vector2.getXY . coordsFromDOT gw gh . AG.positionN $ vrdNode
          nodeId = AG.Id AG.Node id'
          hovered = Set.member nodeId (AG.hoveredElements vrg)
          selected = Set.member nodeId (AG.selectedElements vrg)

onBoth :: (a -> b) -> (a,a) -> (b, b)
onBoth f (x,y) = (f x, f y)

renderEdge :: AG.VRGraph Draw.R -> Int -> AG.VRDEdge Draw.R -> Draw.Image (Maybe AG.Ids)
renderEdge vrg id' vrdEdge = curve `mappend` firstCircle  
  where w = AG.widthG vrg
        h = AG.heightG vrg
        controlPoints = map (coordsFromDOT w h) (AG.pointsE vrdEdge)
        curve = fmap mkIds . Draw.bezierCurve $ map Vector2.getXY controlPoints
        mkIds = mkLabel . Set.singleton $ AG.Id AG.Edge id'
        firstCircle = fmap mkIds 
                      . Draw.tint (Draw.Color 1 0 0 0.5) $
                      (Draw.translate (Vector2.getXY . last $ controlPoints) 
                      Draw.%% Draw.scale 0.02 0.02  
                      Draw.%% Draw.circle)

        
                      
mkLabel :: a -> Any -> Maybe a
mkLabel label (Any b) = if b then Just label else Nothing

-- Temporary hacks
square :: Draw.Image Any
square = Draw.convexPoly [(1,1),(1,-1),(-1,-1),(-1,1)]

nodeBox :: Draw.R -> Draw.R -> AG.Id -> Bool -> Bool -> Draw.Image (Maybe AG.Ids)
nodeBox w h id' hovered selected = fmap label (Draw.tint col (Draw.scale w h Draw.%% circle))
    where n = AG.idNum id'
          c = fromIntegral ((100*n) `mod` 256) / 256
          col = case (hovered, selected) of 
                  (False, False) -> Draw.Color c (1-c) 1 0.5
                  (True, False)  -> Draw.Color 0.8 0.2 0.2 0.8
                  (False, True)  -> Draw.Color 0.2 0.8 0.2 0.8
                  (True,  True)  -> Draw.Color 0.2 0.8 0.2 0.8
          label = mkLabel . Set.singleton $ id'



coordsFromSDL :: (Fractional a) => a -> a -> Vector2.Vector2 a -> Vector2.Vector2 a
coordsFromSDL w h v = Vector2.vector2 (2*(x / w) - 1) (1 - 2*(y / h))
    where x = Vector2.getX v
          y = Vector2.getY v
  
coordsFromSDL' :: (Fractional a) => a -> a -> a -> a -> (a, a)
coordsFromSDL' w h x y = Vector2.getXY . coordsFromSDL w h $ Vector2.vector2 x y

coordsFromDOT :: (Fractional a) => a -> a -> Vector2.Vector2 a -> Vector2.Vector2 a
coordsFromDOT w h v = Vector2.vector2 (2*(x / w) - 1) (2*(y / h) - 1)
    where x = Vector2.getX v
          y = Vector2.getY v
  



locateClick :: (Show c, Integral a, Integral b) => Draw.R -> Draw.R -> a -> b -> Draw.Image c -> c
locateClick w h x y draw = getIds (fromIntegral x) (fromIntegral y) draw
    where getIds x' y' draw' =  do
            let pos = coordsFromSDL' w h x' y'
            Draw.sample draw' pos


circle :: Draw.Image Any
circle = Draw.regularPoly (64 :: Int)
