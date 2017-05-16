module Dev where

import Data.GraphViz (graphToDot, fmtNode, fmtEdge, nonClusteredParams)
import Data.GraphViz.Attributes (toLabel)
import Data.GraphViz.Printing (toDot, renderDot)
import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.Query.BFS (bfs)
import qualified Data.Graph.Inductive.Graph as Graph
import Data.Graph.Inductive.PatriciaTree
import Data.Text.Lazy (unpack)
import qualified Data.Map as M
import qualified Data.List as L

import AttributeGrammar
import Lexer
import Main
import Parser

-- To make it all compile for the moment:
type Analysis a = [a]

{-- How To Run (examples)

-- Strongly Live Variables
ghci> run slv "fib"

--}

slv = undefined
cp  = undefined

run :: (Eq a, Show a) => (Program' -> Analysis a) -> String -> IO ()
run = runAnalysis'

-- run some analysis by passing an analysis function and a 'show' function to display the result
runAnalysis' :: (Eq a, Show a) => (Program' -> Analysis a) -> String -> IO ()
runAnalysis' analyze programName = do
  p <- parse programName
  let t = snd $ (sem_Program p) 0
  let (edges, entryPoint, exitLabels, nodes, _) = sem_Program' t
  let extraNodes = (999999, "Start") : (999998, "End") : nodes
  let extraEdges = (999999, entryPoint, "") : map (\el -> (el, 999998, "")) exitLabels ++ edges
  let cfg = mkGraph extraNodes extraEdges :: Gr String String

  putStrLn "PARSED INPUT:"
  putStrLn (show p)
  putStrLn ""
  putStrLn "LABELLED INPUT:"
  putStrLn (show t)
  putStrLn ""
  putStrLn "CFG:"
  putStrLn $ renderGraph cfg
  putStrLn ""
  putStrLn "REACHABLE CFG:"
  putStrLn $ renderGraph $ reachable 999999 cfg
  putStrLn ""
  putStrLn "ANALYSIS:"
  putStrLn ""
  putStrLn "THE END"
  where
    renderGraph g = let dot = graphToDot params g
                        code = renderDot $ toDot dot
                    in unpack code
    params = nonClusteredParams { fmtNode = fn, fmtEdge = fe }
    fn (_, l) = [toLabel l]
    fe (_, _, l) = [toLabel l]

-- Removes unreachable nodes from the graph.
-- We select all nodes that are reachable from the entry point and discard anything else.
reachable :: Int -> Gr String String -> Gr String String
reachable entryPoint g = let reachableNodes = bfs entryPoint g
                         in  subgraph reachableNodes g

-- parse program

parse :: String -> IO Program
parse programName = do
  let fileName = "../examples/"++programName++".c"
  content <- readFile fileName
  return . happy . alex $ content
