module Dev where

import Data.GraphViz (graphToDot, fmtNode, fmtEdge, nonClusteredParams)
import Data.GraphViz.Attributes (toLabel)
import Data.GraphViz.Printing (toDot, renderDot)
import Data.Graph.Inductive.Graph hiding (Context)
import qualified Data.Graph.Inductive.Graph as Graph
import Data.Graph.Inductive.PatriciaTree
import Data.Text.Lazy (unpack)
import Data.Map (Map)
import qualified Data.Map as M

import AttributeGrammar
import Lexer
import Main
import Monotone
import qualified ConstPropagation as CP
import qualified StronglyLiveVariables as SLV
import Parser

maxContextDepth = 2 -- 0 no context, 1 callsite information, 2 two levels callsite information, ...

run :: String -> IO ()
run = runAnalysis'

-- run some analysis by passing an analysis function and a 'show' function to display the result
runAnalysis' :: String -> IO ()
runAnalysis' programName = do
  p <- parse programName
  let (freshLabel, t) = sem_Program p 0
  let (startLabel, finishLabel) = (freshLabel, freshLabel + 1)
  let (edges, entryPoint, errors, exitLabels, nodes, _) = sem_Program' t
  let extraNodes = (startLabel, S (Skip' startLabel)) : (finishLabel, S (Skip' finishLabel)) : nodes
  let extraEdges = (startLabel, entryPoint, "") : map (\el -> (el, finishLabel, "")) exitLabels ++ edges
  let cfg = mkGraph extraNodes extraEdges :: Gr ProcOrStat String
  let nodeMap = M.fromList extraNodes

  if null errors then do
    putStrLn "CFG:"
    putStrLn $ renderGraph $ nmap show cfg
    putStrLn ""
    putStrLn "CP ANALYSIS:"
    let cpAnalysis = fmap (\(x, y) -> (show x, show y)) (CP.runAnalysis (emap (const ()) cfg) startLabel maxContextDepth)
    putStrLn $ renderAnalysis cpAnalysis nodeMap (map fst extraNodes) extraEdges
    putStrLn ""
    putStrLn "SLV ANALYSIS:"
    let slvAnalysis = fmap (\(x, y) -> (show y, show x)) $ (SLV.runAnalysis (emap (const ()) cfg) finishLabel)
    putStrLn $ renderAnalysis slvAnalysis nodeMap (map fst extraNodes) extraEdges
    putStrLn ""
  else putStrLn $ "Errors: " ++ show errors

renderGraph :: Gr String String -> String
renderGraph g = unpack $ renderDot $ toDot $ graphToDot params g
  where
  params = nonClusteredParams { fmtNode = fn, fmtEdge = fe }
  fn (_, l) = [toLabel l]
  fe (_, _, l) = [toLabel l]

-- Transforms the analysis results in a graph where each node shows the results of the analysis,
-- as well as the statement or procedure in question
renderAnalysis :: Map Int (String, String) -> Map Int ProcOrStat -> [Int] -> [(Int, Int, String)] -> String
renderAnalysis analysis realNodes nodes edges =
  let anNodes = map renderNode nodes
      graph = mkGraph anNodes edges
  in  renderGraph graph
  where
  renderNode label = let (open, closed) = analysis M.! label
                         procOrStat = show $ realNodes M.! label
                     in  (label, unlines [open, procOrStat, closed])

-- parse program
parse :: String -> IO Program
parse programName = do
  let fileName = "../examples/"++programName++".c"
  content <- readFile fileName
  return . happy . alex $ content
