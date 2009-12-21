{-# LANGUAGE GADTs, KindSignatures #-}
module AnnotatedGraph where

import qualified Data.Graph.Inductive as Graph
import qualified Data.Graph.Inductive.PatriciaTree as PTGraph

import qualified Data.IntMap as IntMap
import Data.Monoid
import qualified Data.Set as Set
import qualified Math.Vector2 as Vector2

type GraphStructure a b = PTGraph.Gr a b

type Color = (Double, Double, Double, Double)

newGrNode :: PTGraph.Gr a b -> Graph.Node
newGrNode gr = head (Graph.newNodes 1 gr)

newGrLNode :: a -> PTGraph.Gr a b -> Graph.LNode a
newGrLNode label gr = (newGrNode gr, label)



data ElementType = Node | Edge
     deriving (Ord, Eq, Show)

newtype Id = Id (Set.Set (ElementType, Int))
     deriving (Show, Eq)

newId :: ElementType -> Int -> Id
newId et i = Id . Set.singleton $ (et, i)

instance Monoid Id where
  mempty = Id Set.empty
  mappend (Id is) (Id js) = Id (is `Set.union` js)

data Shape = Rectangle | Ellipse
           deriving (Ord, Eq, Show)

data VRDNode = VRDNode { shapeN :: Shape, 
                         positionN :: Vector2.Vector2 Double, 
                         widthN :: Double,
                         heightN :: Double }
               deriving (Show, Eq)

defaultVRDN :: VRDNode
defaultVRDN = VRDNode { shapeN = Ellipse, 
                        positionN = Vector2.zeroVector,
                        widthN = 10,
                        heightN = 10 }
              
type VRNode = IntMap.IntMap VRDNode

vrNodeEmpty :: VRNode
vrNodeEmpty = IntMap.empty

data VRDEdge = VRDEEmpty | VRDEdge { widthE :: Double, 
                                     pointsE :: [Vector2.Vector2 Double], 
                                     colorE :: Color , 
                                     bezierSamplesE :: Int } 
               deriving (Show,Eq)
                        
type VREdge = IntMap.IntMap VRDEdge

data AnnotatedGraph a b = AG { graph :: GraphStructure a b, vrNodes :: VRNode, vrEdges :: VREdge }

instance Show (AnnotatedGraph a b) where
  show ag = ("(AG: vrNodes = " ++ (show (vrNodes ag)) ++ ")")

empty :: AnnotatedGraph a b
empty = AG { graph = Graph.empty, vrNodes = IntMap.empty, vrEdges = IntMap.empty }

newLNode :: a -> AnnotatedGraph a b -> Graph.LNode a
newLNode label ag = newGrLNode label (graph ag)

insLNode :: Graph.LNode a -> AnnotatedGraph a b -> AnnotatedGraph a b
insLNode n (AG gr nodes edges) = AG (Graph.insNode n gr) (IntMap.insert (fst n) defaultVRDN nodes) edges

insNewLNode :: a -> AnnotatedGraph a b -> AnnotatedGraph a b
insNewLNode x ag = insLNode (newLNode x ag) ag
