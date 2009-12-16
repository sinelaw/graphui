module Render where

import Data.Monoid
import qualified Data.IntMap as IntMap

import qualified AnnotatedGraph

import qualified Control.Arrow as Arrow
import qualified Control.Monad as Monad

import WXFruit
import qualified Graphics.UI.WX as WX
import qualified Graphics.UI.WX.Draw as WXDraw
import qualified FRP.Yampa.Vector2 as Vector2

import Math.Bezier(bezierNSamples)

overlay :: [WXPicture] -> WXPicture
overlay pics = foldr wxPicOver wxblank pics

renderAG :: AnnotatedGraph.AnnotatedGraph a b -> WXPicture
renderAG (AnnotatedGraph.AG g vrNodes vrEdges) = overlay (renderedNodes ++ renderedEdges) where 
    renderedNodes = renderElements vrNodes renderNode
    renderedEdges = renderElements vrEdges renderEdge
    renderElements elMap renderFunc = map (uncurry renderFunc) (IntMap.toList elMap)


rect :: (Num a) => a -> a -> a -> a -> WX.Rect2D a
rect x y w h = WX.rect (WX.point x y) (WX.sz w h)

square :: (Num a) => a -> a -> a -> WX.Rect2D a
square x y a = rect x y a a

wxline :: WX.Point -> WX.Point -> WXPicture
wxline p1 p2 props dc _  = WXDraw.line dc p1 p2 props

yvecToWxPoint :: (RealFloat t) => Vector2.Vector2 t -> WX.Point
yvecToWxPoint v = WX.Point (round . Vector2.vector2X $ v) (round . Vector2.vector2Y $ v)

drawCircle :: (RealFloat a) => Int -> Vector2.Vector2 a -> WXPicture
drawCircle r pos = wxWithColor WX.yellow $ wxPicFill $ wxellipse (square x y r)
                   where (x,y) = (onBoth round) . Vector2.vector2XY $ pos

renderNode :: Int -> AnnotatedGraph.VRDNode -> WXPicture
renderNode jd vrdNode = drawCircle 10 (AnnotatedGraph.positionN vrdNode)

onBoth :: (a->b) -> (a,a) -> (b,b)
onBoth = Monad.join (Arrow.***)  

renderEdge :: Int -> AnnotatedGraph.VRDEdge -> WXPicture
renderEdge jd vrdEdge = overlay (map mkLine (zip intPs (tail intPs))) where
    ps = bezierNSamples (AnnotatedGraph.bezierSamplesE vrdEdge) (map Vector2.vector2XY (AnnotatedGraph.pointsE vrdEdge))
    intPs = map ((uncurry WX.Point) . (onBoth round)) ps
    mkLine = uncurry wxline -- TODO what about line width?
    mkId = const (AnnotatedGraph.newId AnnotatedGraph.Edge jd)
