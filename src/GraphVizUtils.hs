module GraphVizUtils where

import Data.GraphViz

import qualified Data.Graph.Inductive as Graph
import System.IO.Unsafe(unsafePerformIO) 

import Control.Arrow((&&&))
import Data.Maybe(fromJust)
import qualified Data.Map as Map


-- Unsafe IO is safe here because we aren't touching the graphviz random seed 
-- (unsetting it will cause random node assignment, which isn't pure)
-- dotizedGraph'' :: (Graph gr) => Bool -> gr a b
--                                 -> ([GlobalAttributes], gr (AttributeNode a) (AttributeEdge b))
dotizedGraph'' isDir g = unsafePerformIO
                         $ graphToGraph'' isDir g gAttrs noAttrs noAttrs
    where
      gAttrs = []
      noAttrs = const []


-- graphToGraph'' :: (Graph gr) => Bool -> gr a b -> [GlobalAttributes]
--                -> (LNode a -> Attributes) -> (LEdge b -> Attributes)
--                -> ([GlobalAttributes], IO (gr (AttributeNode a) (AttributeEdge b)))
graphToGraph'' isDir gr gAttributes fmtNode fmtEdge
    = dotAttributesAlt isDir gr dot
    where
      dot = graphToDot isDir gr gAttributes fmtNode fmtEdge
      

dotAttributesAlt :: (Graph.Graph gr) => Bool -> gr a b -> DotGraph Graph.Node
                 -> IO ([GlobalAttributes], gr (AttributeNode a) (AttributeEdge b))
dotAttributesAlt isDir gr dot
    = do (Right output) <- graphvizWithHandle command
                                              dot
                                              DotOutput
                                              hGetContents'
         return $ rebuildGraphWithAttributes output
    where
      command = if isDir then dirCommand else undirCommand
      rebuildGraphWithAttributes dotResult =  (gAttrs, Graph.mkGraph lnodes ledges)
          where
            lnodes = map (\(n, l) -> (n, (nodeMap Map.! n, l)))
                     $ Graph.labNodes gr
            ledges = map createEdges $ Graph.labEdges gr
            createEdges (f, t, l) = if isDir || f <= t
                                    then (f, t, getLabel (f,t))
                                    else (f, t, getLabel (t,f))
                where
                  getLabel c = (fromJust $ Map.lookup c edgeMap, l)
            g' = parseDotGraph dotResult
            ns = graphNodes g'
            es = graphEdges g'
            nodeMap = Map.fromList $ map (nodeID &&& nodeAttributes) ns
            edgeMap = Map.fromList $ map ( (edgeFromNodeID &&& edgeToNodeID)
                                           &&& edgeAttributes) es
            gAttrs = attrStmts . graphStatements $ g'