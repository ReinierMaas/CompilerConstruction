{-# LANGUAGE TupleSections #-}

module Dev where

import Data.GraphViz (graphToDot, fmtNode, fmtEdge, nonClusteredParams)
import Data.GraphViz.Attributes (toLabel)
import Data.GraphViz.Printing (toDot, renderDot)
import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.Query.BFS (bfs)
import qualified Data.Graph.Inductive.Graph as Graph
import Data.Graph.Inductive.PatriciaTree
import Data.Text.Lazy (unpack)
import Data.Maybe (mapMaybe)
import Data.Map (Map)
import qualified Data.Map as M
import qualified Data.List as L

import AttributeGrammar
import Lexer
import Main
import Monotone
import Parser

-- To make it all compile for the moment:
type Analysis a = [a]

{-- How To Run (examples)

-- Strongly Live Variables
ghci> run slv "fib"

--}

-- slv :: ((Gr () () -> [Int]), a)
slv = undefined
--  (
--  -- genExtremalLabels
--  \g -> [],
--
--)
cp :: Gr ProcOrStat () -> (Map Int (Maybe (Map String ConstData)), Map Int (Maybe (Map String ConstData)))
cp graph = mfp nodes [999999] constExtremalValue (edges graph) constTransfer constMerge
  where
  nodes :: Map Int Stat' -- (Int, ProcOrStat)
  nodes = M.fromList $ mapMaybe (\(l, ps) -> (l,) <$> getStat ps) $ labNodes graph

run :: (Eq a, Show a) => (Program' -> Analysis a) -> String -> IO ()
run = runAnalysis'

-- run some analysis by passing an analysis function and a 'show' function to display the result
runAnalysis' :: (Eq a, Show a) => (Program' -> Analysis a) -> String -> IO ()
runAnalysis' analyze programName = do
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
    putStrLn "ANALYSIS (OPEN):"
    let (open, closed) = cp $ emap (const ()) cfg
    putStrLn $ renderAnalysis open (map fst extraNodes) extraEdges
    putStrLn "ANALYSIS (CLOSED):"
    putStrLn $ renderAnalysis closed (map fst extraNodes) extraEdges
    putStrLn ""
  else putStrLn $ "Errors: " ++ show errors
  putStrLn "THE END"

renderGraph g = let dot = graphToDot params g
                    code = renderDot $ toDot dot
                in  unpack code
  where
  params = nonClusteredParams { fmtNode = fn, fmtEdge = fe }
  fn (_, l) = [toLabel l]
  fe (_, _, l) = [toLabel l]

renderAnalysis :: Show a => Map Int a -> [Int] -> [(Int, Int, String)] -> String
renderAnalysis analysis nodes edges =
  let anNodes = map (\l -> (l, analysis M.! l)) nodes
      graph = mkGraph anNodes edges
  in  renderGraph $ nmap show $ stupid graph
  where
  -- Necessary to help type inference
  stupid :: Gr a b -> Gr a b
  stupid = id

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
