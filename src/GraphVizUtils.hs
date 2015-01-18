module GraphVizUtils where

import Data.Foldable(foldr)
import Data.GraphViz
import qualified Data.GraphViz.Types.Generalised as GVG
import qualified Data.Graph.Inductive as Graph
import System.IO.Unsafe(unsafePerformIO) 
import qualified Data.Text.Lazy as Text
import System.IO(Handle)
import System.IO.Strict(hGetContents)
import Control.Arrow((&&&))
import Data.Maybe(fromJust)
import qualified Data.Map as Map

import Prelude hiding (foldr)

-- Unsafe IO is safe here because we aren't touching the graphviz random seed 
-- (unsetting it will cause random node assignment, which isn't pure)
dotizedGraph'' :: (Graph.Graph gr) =>
                  gr t b
                  -> Bool
                  -> ([GVG.GlobalAttributes], gr (Attributes, t) (Attributes, b))
dotizedGraph'' g isDir = unsafePerformIO
                         $ graphToGraph'' g isDir gAttrs noAttrs noAttrs
    where
      gAttrs = []
      noAttrs = const []



mkParams :: Bool
            -> [GVG.GlobalAttributes]
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
                  -> IO ([GVG.GlobalAttributes], gr (Attributes, t) (Attributes, b))
graphToGraph'' gr isDir gAttributes fmtNode' fmtEdge'
    = dotAttributesAlt isDir gr dot
    where
      dot = graphToDot params gr
      params = mkParams isDir gAttributes fmtNode' fmtEdge'


getContents' :: Handle -> IO Text.Text
getContents' handle = do
  stuff <- hGetContents handle
  return $ Text.pack stuff

dotAttributesAlt :: (Graph.Graph gr) =>
                    Bool
                    -> gr t b
                    -> DotGraph Graph.Node
                    -> IO ([GVG.GlobalAttributes], gr (Attributes, t) (Attributes, b))
dotAttributesAlt isDir gr dot
    = do output <- graphvizWithHandle command dot DotOutput getContents'
         let parsedC = parseDotGraph output :: GVG.DotGraph Graph.Node
         return $ rebuildGraphWithAttributes parsedC
    where
      command = if isDir then dirCommand else undirCommand
      rebuildGraphWithAttributes g' =  (gAttrs, Graph.mkGraph lnodes ledges)
          where
            lnodes = map (\(n, l) -> (n, (nodeMap Map.! n, l)))
                     $ Graph.labNodes gr
            ledges = map createEdges $ Graph.labEdges gr
            createEdges (f, t, l) = if isDir || f <= t
                                    then (f, t, getLabel (f,t))
                                    else (f, t, getLabel (t,f))
                where
                  getLabel c = (fromJust $ Map.lookup c edgeMap, l)
            nodeMap = Map.fromList $ map (nodeID &&& nodeAttributes) ns
            edgeMap = Map.fromList $ map ( (fromNode &&& toNode)
                                           &&& edgeAttributes) es
            gStmts = GVG.graphStatements g'

            (gAttrs, ns, es) = foldr classifyGraphStatment ([],[],[]) gStmts
            classifyGraphStatment s (gs',ns',es') =
              case s of
               GVG.GA attr -> (attr:gs', ns', es')
               GVG.SG _ -> (gs',ns',es')
               GVG.DN n -> (gs',n:ns',es')
               GVG.DE e -> (gs',ns',e:es')
                                                     
