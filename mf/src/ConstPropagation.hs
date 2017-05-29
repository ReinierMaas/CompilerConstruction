module ConstPropagation (
    extremalValue,
    merge,
    transfer,
    Result(..)
) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)

import AttributeGrammar

data Result = Top
            | Nat Int
            | Bool Bool
            deriving (Eq, Show)

extremalValue :: Map String Result
extremalValue = Map.empty

merge :: Map String Result -> Map String Result -> Map String Result
merge = Map.unionWith mergeSingle
    where
    mergeSingle :: Result -> Result -> Result
    mergeSingle x y | x == y = x
                    | otherwise = Top

transfer :: [(Int, Int)] -> ProcOrStat -> Maybe (Map String Result) -> Maybe (Map String Result)
transfer (S (Call' _ _ _ params out)) = Just $ Map.insert out (eval input (head params))
transfer (S (IAssign' _ name val)) (Just input) = Just $ Map.insert name (evalI input val) input
transfer (S (BAssign' _ name val)) (Just input) = Just $ Map.insert name (evalB input val) input
transfer (S (Malloc' _ name size)) (Just input) = Just $ Map.insert name (evalI input size) input
transfer _ input = input

eval :: Map String Result -> Expr -> Result
eval (I i) = evalI i
eval (B b) = evalB b

evalI :: Map String Result -> IExpr -> Result
evalI input = eval
    where
    combine :: (Int -> Int -> Int) -> Result -> Result -> Result
    combine op (Nat x) (Nat y) = Nat (op x y)
    combine _ _ _ = Top
    eval :: IExpr -> Result
    eval (IConst v) = Nat v
    eval (Var n) = fromMaybe Top (Map.lookup n input)
    eval (Plus x y) = combine (+) (eval x) (eval y)
    eval (Minus x y) = combine (-) (eval x) (eval y)
    eval (Times x y) = combine (*) (eval x) (eval y)
    eval (Divide x y) = combine div (eval x) (eval y)
    eval (Deref x) = eval x

evalB :: Map String Result -> BExpr -> Result
evalB input = eval
    where
    combineB :: (Bool -> Bool -> Bool) -> Result -> Result -> Result
    combineB op (Bool x) (Bool y) = Bool (op x y)
    combineB _ _ _ = Top
    combineI :: (Int -> Int -> Bool) -> Result -> Result -> Result
    combineI op (Nat x) (Nat y) = Bool (op x y)
    combineI _ _ _ = Top
    eval :: BExpr -> Result
    eval (BConst v) = Bool v
    eval (BVar n) = fromMaybe Top (Map.lookup n input)
    eval (LessThan x y) = combineI (<) (evalI input x) (evalI input y)
    eval (GreaterThan x y) = combineI (>) (evalI input x) (evalI input y)
    eval (LessEqual x y) = combineI (<=) (evalI input x) (evalI input y)
    eval (GreaterEqual x y) = combineI (>=) (evalI input x) (evalI input y)
    eval (IEqual x y) = combineI (==) (evalI input x) (evalI input y)
    eval (BEqual x y) = combineB (==) (eval x) (eval y)
    eval (And x y) = combineB (&&) (eval x) (eval y)
    eval (Or x y) = combineB (||) (eval x) (eval y)
    eval (Not x) = notResult (eval x)
    notResult :: Result -> Result
    notResult (Bool x) = Bool (not x)
    notResult x = x
