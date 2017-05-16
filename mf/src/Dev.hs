module Dev where

import Data.GraphViz (graphToDot, fmtNode, fmtEdge, nonClusteredParams)
import Data.GraphViz.Attributes (toLabel)
import Data.GraphViz.Printing (toDot, renderDot)
import Data.Graph.Inductive.Graph
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
  let (edges, _, _, nodes, _) = sem_Program' t
  let cfg = insEdges edges $ insNodes nodes Graph.empty :: Gr String String

  putStrLn "PARSED INPUT:"
  putStrLn (show p)
  putStrLn ""
  putStrLn "TRANSFORMED INPUT:"
  putStrLn (show t)
  putStrLn ""
  putStrLn "CFG:"
  putStrLn $ renderGraph cfg
  putStrLn ""
  putStrLn "Reachable CFG:"
  putStrLn $ renderGraph $ reachable cfg
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
    reachable = id -- FIXME: implement something to remove all unreachable nodes


-- parse program

parse :: String -> IO Program
parse programName = do
  let fileName = "../examples/"++programName++".c"
  content <- readFile fileName
  return . happy . alex $ content
