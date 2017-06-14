-- (C) 2013-14 Pepijn Kokke & Wout Elsinghorst
-- Modifications made Jurriaan Hage
{-# LANGUAGE QuasiQuotes #-}

module Main where

import FunFlow.Lib

import System.Environment (getArgs)
import System.Console.Docopt (Arguments, Docopt, Option, docopt, parseArgs, exitWithUsage, exitWithUsageMessage, getArgOrExitWith, argument, isPresent, command)

import qualified Debug.Trace as Debug

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
