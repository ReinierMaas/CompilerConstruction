{-# LANGUAGE QuasiQuotes #-}

module Main where

import System.Directory (createDirectoryIfMissing, listDirectory)
import System.FilePath (combine, takeBaseName)
import Control.Monad (when)
import Data.Char (toUpper)
import System.Environment (getArgs)
import Data.Text.Lazy (unpack)
import Data.Map (Map)
import qualified Data.Map as M

import System.Console.Docopt (Arguments, Docopt, Option, docopt, parseArgs, exitWithUsage, exitWithUsageMessage, getArgOrExitWith, argument, isPresent, command)
import Data.GraphViz (graphToDot, fmtNode, fmtEdge, nonClusteredParams)
import Data.GraphViz.Attributes (toLabel)
import Data.GraphViz.Printing (toDot, renderDot)
import Data.Graph.Inductive.Graph hiding (Context)
import qualified Data.Graph.Inductive.Graph as Graph
import Data.Graph.Inductive.PatriciaTree

import AttributeGrammar
import Lexer
import Parser
import Monotone
import qualified ConstPropagation as CP
import qualified StronglyLiveVariables as SLV

usage :: Docopt
usage = [docopt|
mf version 0.1.0

Usage:
  mf <max-context-depth> <file>
  mf graphs <max-context-depth> <inputDir> <outputDir>
  mf --help | -h

|]

getArgOrExit :: Arguments -> Option -> IO String
getArgOrExit = getArgOrExitWith usage

parseArgsOrExit :: [String] -> IO Arguments
parseArgsOrExit = either (\err -> exitWithUsageMessage usage (show err)) (\args -> return args) . parseArgs usage

main :: IO ()
main = do
    arguments <- getArgs
    args <- parseArgsOrExit arguments
    if (args `isPresent` (command "--help")) || (args `isPresent` (command "-h"))
    then do
        exitWithUsage usage
    else    if args `isPresent` (command "graphs")
            then do
                maxDepth <- args `getArgOrExit` (argument "max-context-depth")
                inputDir <- args `getArgOrExit` (argument "inputDir")
                outputDir <- args `getArgOrExit` (argument "outputDir")
                inputFiles <- listDirectory inputDir
                createDirectoryIfMissing True outputDir
                sequence_ $ map (\inputFile ->
                    let name = takeBaseName inputFile
                        inputFilePath = (combine inputDir inputFile)
                        cpFilePath = (combine outputDir (name ++ ".cp"))
                        slvFilePath = (combine outputDir (name ++ ".slv"))
                    in  createGraphs (read maxDepth) inputFilePath cpFilePath slvFilePath) inputFiles
            else do
                maxDepth <- args `getArgOrExit` (argument "max-context-depth")
                file <- args `getArgOrExit` (argument "file")
                run (read maxDepth) file

createGraphs :: Int -> String -> String -> String -> IO ()
createGraphs maxDepth inputfile cpgraph slvgraph = do
    p <- happy . alex <$> readFile inputfile
    let (freshLabel, t) = sem_Program p 0
    let (startLabel, finishLabel) = (freshLabel, freshLabel + 1)
    let (edges, entryPoint, errors, exitLabels, nodes, _) = sem_Program' t
    let extraNodes = (startLabel, S (Skip' startLabel)) : (finishLabel, S (Skip' finishLabel)) : nodes
    let extraEdges = (startLabel, entryPoint, "") : map (\el -> (el, finishLabel, "")) exitLabels ++ edges
    let cfg = mkGraph extraNodes extraEdges :: Gr ProcOrStat String
    let nodeMap = M.fromList extraNodes
    if null errors
    then do
        let cpAnalysis = fmap (\(x, y) -> (show x, show y)) (CP.runAnalysis maxDepth (emap (const ()) cfg) startLabel)
        writeFile cpgraph $ renderAnalysis cpAnalysis nodeMap (map fst extraNodes) extraEdges

        let slvAnalysis = fmap (\(x, y) -> (show y, show x)) $ (SLV.runAnalysis (emap (const ()) cfg) finishLabel)
        writeFile slvgraph $ renderAnalysis slvAnalysis nodeMap (map fst extraNodes) extraEdges
    else putStrLn $ "Errors: " ++ show errors

run :: Int -> String -> IO ()
run maxDepth path = do
    p <- happy . alex <$> readFile path
    let (freshLabel, t) = sem_Program p 0
    let (startLabel, finishLabel) = (freshLabel, freshLabel + 1)
    let (edges, entryPoint, errors, exitLabels, nodes, _) = sem_Program' t
    let extraNodes = (startLabel, S (Skip' startLabel)) : (finishLabel, S (Skip' finishLabel)) : nodes
    let extraEdges = (startLabel, entryPoint, "") : map (\el -> (el, finishLabel, "")) exitLabels ++ edges
    let cfg = mkGraph extraNodes extraEdges :: Gr ProcOrStat String
    let nodeMap = M.fromList extraNodes
    if null errors
    then do
        putStrLn "CP ANALYSIS:"
        let cpAnalysis = fmap (\(x, y) -> (show x, show y)) (CP.runAnalysis maxDepth (emap (const ()) cfg) startLabel)
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
