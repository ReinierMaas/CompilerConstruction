-- (C) 2013-14 Pepijn Kokke & Wout Elsinghorst
-- Modifications made Jurriaan Hage
{-# LANGUAGE QuasiQuotes #-}

module Main where

import Ast
import Parsing
import TypeSystem
import Control.Monad.State.Lazy (runState)
import Data.Set (Set)
import Data.Map (Map)

import System.Environment (getArgs)
import System.Console.Docopt (Arguments, Docopt, Option, docopt, parseArgs, exitWithUsage, exitWithUsageMessage, getArgOrExitWith, argument, isPresent, command)

usage :: Docopt
usage = [docopt|
funflow version 0.1.0
Usage:
  funflow <file>
|]

getArgOrExit :: Arguments -> Option -> IO String
getArgOrExit = getArgOrExitWith usage

parseArgsOrExit :: [String] -> IO Arguments
parseArgsOrExit = either (\err -> exitWithUsageMessage usage (show err)) (\args -> return args) . parseArgs usage

main :: IO ()
main = do
    arguments <- getArgs
    args <- parseArgsOrExit arguments
    file <- args `getArgOrExit` (argument "file")
    run file

run :: String -> IO ()
run name = do
  p <- parse name
  let types = runState (w [] p) 0
  putStrLn (show p)
  putStrLn $ show $ fst $ fst types
  return ()

-- |Parse and label program
parse :: String -> IO Expr
parse programName = do
  let fileName = programName++".fun"
  content <- readFile fileName
  return (parseExpr content)


