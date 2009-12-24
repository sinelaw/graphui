{-# LANGUAGE GADTs, KindSignatures, TemplateHaskell, TypeOperators #-}
module AnnotatedGraph where


import qualified Data.Graph.Inductive as Graph
--import qualified Data.Graph.Inductive.PatriciaTree as PTGraph

import qualified Data.IntMap as IntMap
import Data.Monoid
import qualified Data.Set as Set
import qualified Math.Vector2 as Vector2

-- fclabel stuff
import Prelude hiding ((.),id,mod)
import Control.Category
import Data.Record.Label
--

type GraphStructure a b = Graph.Gr a b

type Color = (Double, Double, Double, Double)

newGrNode :: Graph.Gr a b -> Graph.Node
newGrNode = head . Graph.newNodes 1

newGrLNode :: a -> Graph.Gr a b -> Graph.LNode a
newGrLNode label' gr = (newGrNode gr, label')

newGrEdgeNum :: Graph.Gr a (Int, b) -> Int
newGrEdgeNum gr = 1 + foldr (\(_,_,(i,_)) prev -> max prev i) 0 (Graph.labEdges gr)

newGrLEdge :: Int -> Int -> b -> Graph.Gr a (Int, b) -> Graph.LEdge (Int, b)
newGrLEdge n1 n2 label' gr = (n1, n2, (newGrEdgeNum gr, label'))

data ElementType = Node | Edge
     deriving (Ord, Eq, Show)

data Id = Id ElementType Int
     deriving (Show, Eq, Ord)

type Ids = Set.Set Id

idIsElement :: ElementType -> Id -> Bool
idIsElement et (Id et' _) =  et == et'


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
                                     bezierSamplesE  :: [Vector2.Vector2 Double], 
                                     bezierSamplesNumE :: Int } 
               deriving (Show,Eq)
                        
defaultVRDE :: VRDEdge
defaultVRDE = VRDEdge { widthE = 0,
                        pointsE = replicate 4 Vector2.zeroVector,
                        bezierSamplesNumE = 8,
                        bezierSamplesE = [] }

type VREdge = IntMap.IntMap VRDEdge

vrEdgeEmpty :: VREdge
vrEdgeEmpty = IntMap.empty

data VRGraph = VRGraph { mousePos :: Vector2.Vector2 Double, 
                         needsLayout :: Bool, 
                         renderGraph :: Bool,
                         selectedElements :: Ids, 
                         widthG :: Double,
                         heightG :: Double,
                         zoomG :: Double}
               deriving (Show)
  
defaultVRG :: VRGraph
defaultVRG = VRGraph{mousePos = Vector2.zeroVector, 
                     renderGraph = True,
                     needsLayout = False, 
                     selectedElements = mempty, 
                     widthG = 1, 
                     heightG = 1, 
                     zoomG = 1} 
             
data AnnotatedGraph a b = AG { graph :: GraphStructure a (Int, b), 
                               vrNodes :: VRNode, 
                               vrEdges :: VREdge, 
                               vrGraph :: VRGraph}

-- Use fclabels to make nicer field accessors
$(mkLabels [''AnnotatedGraph, ''VRNode, ''VREdge, ''VRGraph])
  
  
lZoomG :: VRGraph :-> Double  
lHeightG :: VRGraph :-> Double
lWidthG :: VRGraph :-> Double
lSelectedElements :: VRGraph :-> Ids
lRenderGraph :: VRGraph :-> Bool
lNeedsLayout :: VRGraph :-> Bool
lMousePos :: VRGraph :-> Vector2.Vector2 Double
lVrGraph :: AnnotatedGraph a b :-> VRGraph
lVrEdges :: AnnotatedGraph a b :-> VREdge                                        
lVrNodes :: AnnotatedGraph a b :-> VRNode                                        
lGraph :: AnnotatedGraph a b :-> GraphStructure a (Int, b) 
                                      
zoomBy :: Double -> AnnotatedGraph a b -> AnnotatedGraph a b
zoomBy s ag = set (lZoomG . lVrGraph) (s * (get (lZoomG . lVrGraph) $ ag)) ag

toggleRender :: AnnotatedGraph a b -> AnnotatedGraph a b
toggleRender ag = set (lRenderGraph . lVrGraph) (not . get (lRenderGraph . lVrGraph) $ ag) ag

setNeedsLayout :: Bool -> AnnotatedGraph a b -> AnnotatedGraph a b
setNeedsLayout = set (lNeedsLayout . lVrGraph)

setMousePos :: Vector2.Vector2 Double -> AnnotatedGraph a b -> AnnotatedGraph a b
setMousePos = set (lMousePos . lVrGraph)

setSelectedElements :: Ids -> AnnotatedGraph a b -> AnnotatedGraph a b
setSelectedElements = set (lSelectedElements . lVrGraph)

resetSelectedElements :: AnnotatedGraph a b -> AnnotatedGraph a b
resetSelectedElements = setSelectedElements Set.empty

instance Show (AnnotatedGraph a b) where
  show ag = "(AG: vrGraph = " ++ show (vrGraph ag)  
            ++ " ;\n\tvrNodes = " ++ show (vrNodes ag)  
            ++ " ;\n\tvrEdges = " ++ show (vrEdges ag)  
            ++ " ;\n\tnodes = " ++ show (Graph.nodes (graph ag)) 
            ++ " ;\n\tedges = " ++ show (Graph.edges (graph ag)) 
            ++ ")"

empty :: AnnotatedGraph a b
empty = AG { graph = Graph.empty, 
             vrNodes = IntMap.empty, 
             vrEdges = vrEdgeEmpty, 
             vrGraph = defaultVRG
           }

newLNode :: a -> AnnotatedGraph a b -> Graph.LNode a
newLNode label' = newGrLNode label' . graph

insLNode :: Graph.LNode a -> AnnotatedGraph a b -> AnnotatedGraph a b
insLNode n ag = set lVrNodes newVRNodes updatedGr
    where updatedGr = set lGraph (Graph.insNode n (graph ag)) ag
          newVRNodes = IntMap.insert (fst n) defaultVRDN (vrNodes ag)

insNewLNode :: a -> AnnotatedGraph a b -> AnnotatedGraph a b
insNewLNode x ag = insLNode (newLNode x ag) ag


newLEdge :: Int -> Int -> b -> AnnotatedGraph a b -> Graph.LEdge (Int, b)
newLEdge n1 n2 label' = newGrLEdge n1 n2 label' . graph

insLEdge :: Graph.LEdge (Int,b) -> AnnotatedGraph a b -> AnnotatedGraph a b
insLEdge eg@(_,_,(id',_)) ag = set lVrEdges newVREdges updatedGr
  where updatedGr = set lGraph (Graph.insEdge eg (graph ag)) ag
        newVREdges = IntMap.insert id' defaultVRDE (vrEdges ag)

insNewLEdge :: Int -> Int -> b -> AnnotatedGraph a b -> AnnotatedGraph a b
insNewLEdge n1 n2 label' ag = insLEdge (newLEdge n1 n2 label' ag) ag

connectNodes :: [Id] -> b -> AnnotatedGraph a b -> AnnotatedGraph a b
connectNodes nList label' ag = foldr connect' (setNeedsLayout True ag) nodePairs
    where nodePairs = zip nList (tail nList)
          connect' (Id Node n1, Id Node n2) = insNewLEdge n1 n2 label'
          connect' _ = error "Expecting node IDs when connecting a new edge"