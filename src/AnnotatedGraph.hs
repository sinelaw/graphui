{-# LANGUAGE GADTs, KindSignatures #-}
module AnnotatedGraph where

import qualified Graphics.DrawingCombinators as Draw
import qualified Data.Graph.Inductive as Graph
import qualified Data.Graph.Inductive.PatriciaTree as PTGraph

import qualified Data.IntMap as IntMap
import Data.Monoid
import qualified Data.Set as Set

type GraphStructure a b = PTGraph.Gr a b

newGrNode :: PTGraph.Gr a b -> Graph.Node
newGrNode gr = head (Graph.newNodes 1 gr)

newGrLNode :: a -> PTGraph.Gr a b -> Graph.LNode a
newGrLNode label gr = (newGrNode gr, label)



data ElementType = Node | Edge
     deriving (Ord, Eq, Show)

data Id = Id (Set.Set (ElementType, Int))
     deriving (Show)

newId :: ElementType -> Int -> Id
newId et i = Id . Set.singleton $ (et, i)

instance Monoid Id where
  mempty = Id Set.empty
  mappend (Id is) (Id js) = Id (is `Set.union` js)

data Shape = Rectangle | Ellipse

data VRDNode = VRDNEmpty | VRDNode { shapeN :: Shape, positionN, scaleN :: Draw.Vec2, colorN :: Draw.Color }
type VRNode = IntMap.IntMap VRDNode

data VRDEdge = VRDEEmpty | VRDEdge { widthE :: Double, pointsE :: [Draw.Vec2], colorE :: Draw.Color , bezierSamplesE :: Int } 
type VREdge = IntMap.IntMap VRDEdge

data AnnotatedGraph a b = AG { graph :: GraphStructure a b, vrNodes :: VRNode, vrEdges :: VREdge }


newLNode :: a -> AnnotatedGraph a b -> Graph.LNode a
newLNode label ag = newGrLNode label (graph ag)

insLNode :: Graph.LNode a -> AnnotatedGraph a b -> AnnotatedGraph a b
insLNode n (AG gr nodes edges) = AG (Graph.insNode n gr) (IntMap.insert (fst n) VRDNEmpty nodes) edges

insNewLNode :: a -> AnnotatedGraph a b -> AnnotatedGraph a b
insNewLNode x ag = insLNode (newLNode x ag) ag
