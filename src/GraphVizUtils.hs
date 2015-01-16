module GraphVizUtils where

import Data.GraphViz.Types.Generalised
import Data.GraphViz(dirCommand,undirCommand,parseDotGraph,GraphvizOutput(..),GraphvizParams(..),nonClusteredParams,fmtNode,fmtEdge,isDirected,graphToDot,PrintDotRepr(..),graphEdges,graphNodes,attrStmts,graphvizWithHandle,Attributes,DotRepr(..))

import qualified Data.Graph.Inductive as Graph
import System.IO.Unsafe(unsafePerformIO) 
import Data.Text.Lazy(pack)
import System.IO(hGetContents)
import Control.Arrow((&&&))
import Data.Maybe(fromJust)
import qualified Data.Map as Map


-- Unsafe IO is safe here because we aren't touching the graphviz random seed 
-- (unsetting it will cause random node assignment, which isn't pure)
dotizedGraph'' :: (Graph.Graph gr) =>
                  gr t b
                  -> Bool
                  -> ([GlobalAttributes], gr (Attributes, t) (Attributes, b))
dotizedGraph'' g isDir = unsafePerformIO
                         $ graphToGraph'' g isDir gAttrs noAttrs noAttrs
    where
      gAttrs = []
      noAttrs = const []



mkParams :: Bool
            -> [GlobalAttributes]
            -> ((n, l) -> Attributes)
            -> ((n, n, el) -> Attributes)
            -> GraphvizParams n l el () l
mkParams isDir gAttributes fmtNode' fmtEdge' =
  nonClusteredParams { fmtNode = fmtNode'
                     , fmtEdge = fmtEdge'
                     , isDirected = isDir
                     , globalAttributes = gAttributes
                     }

graphToGraph'' :: (Graph.Graph gr) =>
                  gr t b
                  -> Bool
                  -> [GlobalAttributes]
                  -> ((Graph.Node, t) -> Attributes)
                  -> ((Graph.Node, Graph.Node, b) -> Attributes)
                  -> IO ([GlobalAttributes], gr (Attributes, t) (Attributes, b))
graphToGraph'' gr isDir gAttributes fmtNode' fmtEdge'
    = dotAttributesAlt isDir gr dot
    where
      dot = graphToDot params gr
      params = mkParams isDir gAttributes fmtNode' fmtEdge'


-- dotAttributesAlt :: (Graph.Graph gr) => Bool -> gr a b -> DotGraph Graph.Node
--                  -> IO ([GlobalAttributes], gr (AttributeNode a) (AttributeEdge b))
-- dotAttributesAlt :: (PrintDotRepr dg n, Graph.Graph gr1, Graph.Graph gr) =>
--                     Bool
--                     -> gr t b
--                     -> dg n
--                     -> IO ([GlobalAttributes], gr1 (Attributes, t) (Attributes, b))
dotAttributesAlt isDir gr dot
    = do output <- graphvizWithHandle command
                                              dot
                                              DotOutput
                                              hGetContents
         return $ rebuildGraphWithAttributes $ pack output
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
            edgeMap = Map.fromList $ map ( (fromNode &&& toNode)
                                           &&& edgeAttributes) es
--             (gAttrs, _) = graphStructureInformation $ g'
            gAttrs = attrStmts . graphStatements $ g'
--       rebuildGraphWithAttributes dotResult =  ([gAttrs], Graph.mkGraph lnodes ledges)
--           where
--             lnodes = map (\(n, l) -> (n, (nodeMap Map.! n, l)))
--                      $ Graph.labNodes gr
--             ledges = map createEdges $ Graph.labEdges gr
--             createEdges (f, t, l) = if isDir || f <= t
--                                     then (f, t, getLabel (f,t))
--                                     else (f, t, getLabel (t,f))
--                 where
--                   getLabel c = (fromJust $ Map.lookup c edgeMap, l)
--             g' = parseDotGraph dotResult
--             nodeMap = Map.map snd $ nodeInformation True g'
--             es = edgeInformation True g'
-- --            nodeMap = Map.fromList $ map (nodeID &&& nodeAttributes) ns
--             edgeMap = Map.fromList $ map ( (fromNode &&& toNode)
--                                            &&& edgeAttributes) es
--             (gAttrs, _) = graphStructureInformation $ g'
