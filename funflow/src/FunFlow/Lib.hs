module FunFlow.Lib where

import Control.Monad.State.Lazy (evalState, runState)
import qualified Data.Map.Strict as Map

import FunFlow.Ast
import FunFlow.Labeling
import FunFlow.Parsing
import FunFlow.TypeSystem

run :: String -> IO ()
run name = putStrLn =<< show <$> parse name

typeCheck :: Expr -> (Type, Constraints, Expr)
typeCheck p =
  -- Note: it is extremely important that the internal state of label gets passed on
  -- to w. Otherwise, it would be possible for pi collisions to occur.
  let (p', fresh) = runState (label p) 0
      (ty, _, constraints) = evalState (w Map.empty p') fresh
  in  (ty, constraints, p')

-- |Parse and label program
parse :: String -> IO Expr
parse fileName = do
  content <- readFile fileName
  return (parseExpr content)
