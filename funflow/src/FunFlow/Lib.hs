module FunFlow.Lib where

import Control.Monad.State.Lazy (runState)
import qualified Data.Map.Strict as Map

import FunFlow.Ast
import FunFlow.Parsing
import FunFlow.TypeSystem

run :: String -> IO ()
run name = putStrLn =<< show <$> typeCheck name

typeCheck :: String -> IO Type
typeCheck name = do
  p <- parse name
  putStrLn (show p)
  let types = runState (w Map.empty p) 0
  return $ fst $ fst types

-- |Parse and label program
parse :: String -> IO Expr
parse fileName = do
  content <- readFile fileName
  return (parseExpr content)
