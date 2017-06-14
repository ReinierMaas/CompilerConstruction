module FunFlow.Lib where

import Control.Monad.State.Lazy (evalState)
import qualified Data.Map.Strict as Map

import FunFlow.Ast
import FunFlow.Labeling
import FunFlow.Parsing
import FunFlow.TypeSystem

run :: String -> IO ()
run name = putStrLn =<< show <$> typeCheck name

typeCheck :: String -> IO Type
typeCheck name = do
  p <- parse name
  -- Note: it is extremely important that the internal state of label gets passed on
  -- to w. Otherwise, it would be possible for pi collisions to occur.
  let types = evalState (w Map.empty =<< label p) 0
  return $ fst types

-- |Parse and label program
parse :: String -> IO Expr
parse fileName = do
  content <- readFile fileName
  return (parseExpr content)
