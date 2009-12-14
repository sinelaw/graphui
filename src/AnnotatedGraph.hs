{-# LANGUAGE GADTs, KindSignatures #-}
module AnnotatedGraph where

import qualified Graphics.DrawingCombinators as Draw
import qualified Data.Graph.Inductive as Graph
import qualified Data.Map as Map
import Data.Monoid

type GraphStructure = Graph.Gr String String


data ElementType = Node | Edge

data Id = Id [(ElementType, Int)]

instance Monoid Id where
  mempty = Id []
  mappend (Id is) (Id js) = Id (is ++ js)

data Shape = Rectangle | Ellipse

data VRDNode = VRDNode { shapeN :: Shape, positionN, scaleN :: Draw.Vec2, colorN :: Draw.Color }
type VRNode = Map.Map Int VRDNode

data VRDEdge = VRDEdge { widthE :: Double, pointsE :: [Draw.Vec2], colorE :: Draw.Color } 
type VREdge = Map.Map Int VRDEdge

data AnnotatedGraph = AG { graph :: GraphStructure, vrNodes :: VRNode, vrEdges :: VREdge }



