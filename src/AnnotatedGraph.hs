{-# LANGUAGE GADTs, KindSignatures, TemplateHaskell, TypeOperators #-}
module AnnotatedGraph where


import qualified Data.Graph.Inductive as Graph
import qualified Data.Graph.Inductive.PatriciaTree as PTGraph

import qualified Data.IntMap as IntMap
import Data.Monoid
import qualified Data.Set as Set
import qualified Math.Vector2 as Vector2

-- fclabel stuff
import Prelude hiding ((.),id,mod)
import Control.Category
import Data.Record.Label
--

type GraphStructure a b = PTGraph.Gr a b

type Color = (Double, Double, Double, Double)

newGrNode :: PTGraph.Gr a b -> Graph.Node
newGrNode gr = head (Graph.newNodes 1 gr)

-- newGrEdge :: PTGraph.Gr a b -> Graph.Edge
-- newGrEdge gr = 1 + (foldr (\prev (i,label') -> max 0 (Graph.edges gr))

newGrLNode :: a -> PTGraph.Gr a b -> Graph.LNode a
newGrLNode label' gr = (newGrNode gr, label')

-- newGrLEdge :: b -> PTGraph.Gr a b -> Graph.LEdge b
-- newGrLEdge n1 n2 label' gr = (newGrEdge gr, label')

data ElementType = Node | Edge
     deriving (Ord, Eq, Show)

data Id = Id ElementType Int
     deriving (Show, Eq, Ord)

type Ids = Set.Set Id

idIsElement :: ElementType -> Id -> Bool
idIsElement et (Id et' n) = (et == et')


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

data VRGraph = VRGraph { mousePos :: Vector2.Vector2 Double, needsLayout :: Bool, selectedElements :: Set.Set Id }
  
data AnnotatedGraph a b = AG { graph :: GraphStructure a b, vrNodes :: VRNode, vrEdges :: VREdge, vrGraph :: VRGraph}

-- Use fclabels to make nicer field accessors
$(mkLabels [''AnnotatedGraph, ''VRNode, ''VREdge, ''VRGraph])
  
lSelectedElements :: VRGraph :-> Set.Set Id
lNeedsLayout :: VRGraph :-> Bool
lMousePos :: VRGraph :-> Vector2.Vector2 Double
lVrGraph :: AnnotatedGraph a b :-> VRGraph
lVrEdges :: AnnotatedGraph a b :-> VREdge                                        
lVrNodes :: AnnotatedGraph a b :-> VRNode                                        
lGraph :: AnnotatedGraph a b :-> GraphStructure a b                                        
                                      
setNeedsLayout :: Bool -> AnnotatedGraph a b -> AnnotatedGraph a b
setNeedsLayout = set (lNeedsLayout . lVrGraph)

setMousePos :: Vector2.Vector2 Double -> AnnotatedGraph a b -> AnnotatedGraph a b
setMousePos = set (lMousePos . lVrGraph)

setSelectedElements :: Set.Set Id -> AnnotatedGraph a b -> AnnotatedGraph a b
setSelectedElements = set (lSelectedElements . lVrGraph)

instance Show (AnnotatedGraph a b) where
  show ag = ("(AG: vrNodes = " ++ (show (vrNodes ag)) ++ ")")

empty :: AnnotatedGraph a b
empty = AG { graph = Graph.empty, 
             vrNodes = IntMap.empty, 
             vrEdges = IntMap.empty, 
             vrGraph = VRGraph{mousePos = Vector2.zeroVector, needsLayout = False, selectedElements = mempty } 
           }

newLNode :: a -> AnnotatedGraph a b -> Graph.LNode a
newLNode label' ag = newGrLNode label' (graph ag)

insLNode :: Graph.LNode a -> AnnotatedGraph a b -> AnnotatedGraph a b
insLNode n ag = AG{ graph = (Graph.insNode n (graph ag)), 
                    vrNodes = (IntMap.insert (fst n) defaultVRDN (vrNodes ag)),
                    vrEdges = (vrEdges ag),
                    vrGraph = (vrGraph ag)}

insNewLNode :: a -> AnnotatedGraph a b -> AnnotatedGraph a b
insNewLNode x ag = insLNode (newLNode x ag) ag


-- newLEdge :: b -> AnnotatedGraph a b -> Graph.LEdge b
-- newLEdge label' ag = newGrLEdge label' (graph ag)

-- insLEdge :: Graph.LEdge b -> AnnotatedGraph a b -> AnnotatedGraph a b
-- insLEdge id' n1 n2 ag = 

-- connectNodes :: Ids -> AnnotatedGraph a b -> AnnotatedGraph a b
-- connectNodes nodeIds ag = Graph.insEdge