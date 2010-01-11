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

newtype (RealFloat a) => Color a = Color (a,a,a,a)

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

data VRDNode a = VRDNode { shapeN    :: Shape, 
                           positionN :: Vector2.Vector2 a, 
                           widthN    :: a,
                           heightN   :: a }
                 deriving (Show, Eq)

defaultVRDN :: (RealFloat a) => VRDNode a
defaultVRDN = VRDNode { shapeN = Ellipse, 
                        positionN = Vector2.zeroVector,
                        widthN = 10,
                        heightN = 10 }
              
type VRNode a = IntMap.IntMap (VRDNode a)

vrNodeEmpty :: VRNode a
vrNodeEmpty = IntMap.empty

data VRDEdge a = VRDEEmpty | VRDEdge { widthE :: a, 
                                       pointsE :: [Vector2.Vector2 a], 
                                       bezierSamplesE  :: [Vector2.Vector2 a], 
                                       bezierSamplesNumE :: Int } 
                 deriving (Show,Eq)
                        
defaultVRDE :: (RealFloat a) => VRDEdge a
defaultVRDE = VRDEdge { widthE = 0,
                        pointsE = replicate 4 Vector2.zeroVector,
                        bezierSamplesNumE = 8,
                        bezierSamplesE = [] }

type VREdge a = IntMap.IntMap (VRDEdge a)

vrEdgeEmpty :: VREdge a
vrEdgeEmpty = IntMap.empty

data VRGraph a = VRGraph { mousePos :: Vector2.Vector2 a, 
                           needsLayout :: Bool, 
                           renderGraph :: Bool,
                           selectedElements :: Ids, 
                           widthG :: a,
                           heightG :: a,
                           zoomG :: a}
               deriving (Show)
  
defaultVRG :: (RealFloat a) => VRGraph a
defaultVRG = VRGraph{mousePos = Vector2.zeroVector, 
                     renderGraph = True,
                     needsLayout = False, 
                     selectedElements = mempty, 
                     widthG = 1, 
                     heightG = 1, 
                     zoomG = 1} 
             
data AnnotatedGraph a b c = AG { graph :: GraphStructure a (Int, b), 
                                 vrNodes :: VRNode c, 
                                 vrEdges :: VREdge c, 
                                 vrGraph :: VRGraph c}

-- Use fclabels to make nicer field accessors
$(mkLabels [''AnnotatedGraph, ''VRNode, ''VREdge, ''VRGraph])
  
  
lZoomG :: VRGraph a :-> a
lHeightG :: VRGraph a :-> a
lWidthG :: VRGraph a :-> a
lSelectedElements :: VRGraph a :-> Ids
lRenderGraph :: VRGraph a :-> Bool
lNeedsLayout :: VRGraph a :-> Bool
lMousePos :: VRGraph a :-> Vector2.Vector2 a
lVrGraph :: AnnotatedGraph a b c :-> VRGraph c
lVrEdges :: AnnotatedGraph a b c :-> VREdge c
lVrNodes :: AnnotatedGraph a b c :-> VRNode c
lGraph :: AnnotatedGraph a b c :-> GraphStructure a (Int, b) 
                                      
zoomBy :: (Num c) => c -> AnnotatedGraph a b c -> AnnotatedGraph a b c
zoomBy s ag = set (lZoomG . lVrGraph) (s * (get (lZoomG . lVrGraph) ag)) ag

toggleRender :: AnnotatedGraph a b c -> AnnotatedGraph a b c
toggleRender ag = set (lRenderGraph . lVrGraph) (not . get (lRenderGraph . lVrGraph) $ ag) ag

setNeedsLayout :: Bool -> AnnotatedGraph a b c -> AnnotatedGraph a b c
setNeedsLayout = set (lNeedsLayout . lVrGraph)

setMousePos :: Vector2.Vector2 c -> AnnotatedGraph a b c -> AnnotatedGraph a b c
setMousePos = set (lMousePos . lVrGraph)

setSelectedElements :: Ids -> AnnotatedGraph a b c -> AnnotatedGraph a b c
setSelectedElements = set (lSelectedElements . lVrGraph)

resetSelectedElements :: AnnotatedGraph a b c -> AnnotatedGraph a b c
resetSelectedElements = setSelectedElements Set.empty

instance (Show c) => Show (AnnotatedGraph a b c) where
  show ag = "(AG: vrGraph = " ++ show (vrGraph ag)  
            ++ " ;\n\tvrNodes = " ++ show (vrNodes ag)  
            ++ " ;\n\tvrEdges = " ++ show (vrEdges ag)  
            ++ " ;\n\tnodes = " ++ show (Graph.nodes (graph ag)) 
            ++ " ;\n\tedges = " ++ show (Graph.edges (graph ag)) 
            ++ ")"

empty :: (RealFloat c) => AnnotatedGraph a b c
empty = AG { graph = Graph.empty, 
             vrNodes = IntMap.empty, 
             vrEdges = vrEdgeEmpty, 
             vrGraph = defaultVRG
           }

newLNode :: a -> AnnotatedGraph a b c -> Graph.LNode a
newLNode label' = newGrLNode label' . graph

insLNode :: (RealFloat c) => Graph.LNode a -> AnnotatedGraph a b c -> AnnotatedGraph a b c
insLNode n ag = set lVrNodes newVRNodes updatedGr
    where updatedGr = set lGraph (Graph.insNode n (graph ag)) ag
          newVRNodes = IntMap.insert (fst n) defaultVRDN (vrNodes ag)

insNewLNode :: (RealFloat c) => a -> AnnotatedGraph a b c -> AnnotatedGraph a b c
insNewLNode x ag = insLNode (newLNode x ag) ag


newLEdge :: Int -> Int -> b -> AnnotatedGraph a b c -> Graph.LEdge (Int, b)
newLEdge n1 n2 label' = newGrLEdge n1 n2 label' . graph

insLEdge :: (RealFloat c) => Graph.LEdge (Int,b) -> AnnotatedGraph a b c -> AnnotatedGraph a b c
insLEdge eg@(_,_,(id',_)) ag = set lVrEdges newVREdges updatedGr
  where updatedGr = set lGraph (Graph.insEdge eg (graph ag)) ag
        newVREdges = IntMap.insert id' defaultVRDE (vrEdges ag)

insNewLEdge :: (RealFloat c) => Int -> Int -> b -> AnnotatedGraph a b c -> AnnotatedGraph a b c
insNewLEdge n1 n2 label' ag = insLEdge (newLEdge n1 n2 label' ag) ag

connectNodes :: (RealFloat c) => [Id] -> b -> AnnotatedGraph a b c -> AnnotatedGraph a b c
connectNodes nList label' ag = foldr connect' (setNeedsLayout True ag) nodePairs
    where nodePairs = zip nList (tail nList)
          connect' (Id Node n1, Id Node n2) = insNewLEdge n1 n2 label'
          connect' _ = error "Expecting node IDs when connecting a new edge"