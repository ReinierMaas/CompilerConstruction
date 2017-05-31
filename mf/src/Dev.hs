{-# LANGUAGE TupleSections #-}

module Dev where

import Data.GraphViz (graphToDot, fmtNode, fmtEdge, nonClusteredParams)
import Data.GraphViz.Attributes (toLabel)
import Data.GraphViz.Printing (toDot, renderDot)
import Data.Graph.Inductive.Graph hiding (Context)
import Data.Graph.Inductive.Query.BFS (bfs)
import qualified Data.Graph.Inductive.Graph as Graph
import Data.Graph.Inductive.PatriciaTree
import Data.Text.Lazy (unpack)
import Data.Maybe (mapMaybe)
import Data.Set (Set)
import qualified Data.Set as S
import Data.Map (Map)
import qualified Data.Map as M
import qualified Data.List as L
import Data.Tuple (swap)

import AttributeGrammar
import Lexer
import Main
import Monotone
import qualified ConstPropagation as CP
import qualified StronglyLiveVariables as SLV
import Parser

cp :: Gr ProcOrStat () -> Map Int ([(Context, Map String CP.Result)], [(Context, Map String CP.Result)])
cp graph = mfp (nodes' graph) [999999] CP.extremalValue (edges graph) CP.unaryTransfer CP.binaryTransfer CP.merge

--slv :: Gr ProcOrStat () -> Map Int (Maybe (Set String), Maybe (Set String))
--slv graph = mfp (nodes' graph) [999998] SLV.extremalValue (map swap (edges graph)) SLV.transfer SLV.merge

nodes' :: Gr ProcOrStat () -> Map Int ProcOrStat
nodes' = M.fromList . labNodes

run :: String -> IO ()
run = runAnalysis'

-- run some analysis by passing an analysis function and a 'show' function to display the result
runAnalysis' :: String -> IO ()
runAnalysis' programName = do
  p <- parse programName
  let t = snd $ (sem_Program p) 0
  let (edges, entryPoint, errors, exitLabels, nodes, _) = sem_Program' t
  let extraNodes = (999999, S (Skip' 999999)) : (999998, S (Skip' 999998)) : nodes
  let extraEdges = (999999, entryPoint, "") : map (\el -> (el, 999998, "")) exitLabels ++ edges
  let cfg = mkGraph extraNodes extraEdges :: Gr ProcOrStat String

  putStrLn "PARSED INPUT:"
  putStrLn (show p)
  putStrLn ""
  putStrLn "LABELLED INPUT:"
  putStrLn (show t)
  putStrLn ""
  if null errors then do
    putStrLn "CFG:"
    putStrLn $ renderGraph $ nmap show cfg
    putStrLn ""
    putStrLn "REACHABLE CFG:"
    putStrLn $ renderGraph $ nmap show $ reachable 999999 cfg
    putStrLn ""
    putStrLn "CP ANALYSIS:"
    let cpAnalysis = fmap (\(x, y) -> show x ++ "\n" ++ show y) $ cp $ emap (const ()) cfg
    putStrLn $ renderAnalysis cpAnalysis (map fst extraNodes) extraEdges
    putStrLn ""
    --putStrLn "SLV ANALYSIS:"
    --let slvAnalysis = fmap (\(x, y) -> showJust' y ++ "\n" ++ showJust' x) $ slv $ emap (const ()) cfg
    --putStrLn $ renderAnalysis slvAnalysis (map fst extraNodes) extraEdges
    --putStrLn ""
  else putStrLn $ "Errors: " ++ show errors
  putStrLn "THE END"

renderGraph :: Gr String String -> String
renderGraph g = let dot = graphToDot params g
                    code = renderDot $ toDot dot
                in  unpack code
  where
  params = nonClusteredParams { fmtNode = fn, fmtEdge = fe }
  fn (_, l) = [toLabel l]
  fe (_, _, l) = [toLabel l]

renderAnalysis :: Map Int String -> [Int] -> [(Int, Int, String)] -> String
renderAnalysis analysis nodes edges =
  let anNodes = map (\l -> (l, analysis M.! l)) nodes
      graph = mkGraph anNodes edges
  in  renderGraph graph

-- Removes unreachable nodes from the graph.
-- We select all nodes that are reachable from the entry point and discard anything else.
reachable :: Int -> Gr a b -> Gr a b
reachable entryPoint g = let reachableNodes = bfs entryPoint g
                         in  subgraph reachableNodes g

-- parse program

parse :: String -> IO Program
parse programName = do
  let fileName = "../examples/"++programName++".c"
  content <- readFile fileName
  return . happy . alex $ content
