module ConstPropagation where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)

import AttributeGrammar

data ConstData = Top
               | Nat Int
               | Bool Bool
               deriving (Eq, Show)

constExtremalValue :: Map String ConstData
constExtremalValue = Map.empty

constMerge :: Map String ConstData -> Map String ConstData -> Map String ConstData
constMerge = Map.unionWith mergeSingle
    where
    mergeSingle :: ConstData -> ConstData -> ConstData
    mergeSingle x y | x == y = x
                    | otherwise = Top

constTransfer :: Stat' -> Maybe (Map String ConstData) -> Maybe (Map String ConstData)
constTransfer (IAssign' _ name val) (Just input) = Just $ Map.insert name (evalI input val) input
constTransfer (BAssign' l name val) (Just input) = Just $ Map.insert name (evalB input val) input
constTransfer _ input = input

evalI :: Map String ConstData -> IExpr -> ConstData
evalI input = eval
    where
    combine :: (Int -> Int -> Int) -> ConstData -> ConstData -> ConstData
    combine op (Nat x) (Nat y) = Nat (op x y)
    combine _ _ _ = Top
    eval :: IExpr -> ConstData
    eval (IConst v) = Nat v
    eval (Var n) = fromMaybe Top (Map.lookup n input)
    eval (Plus x y) = combine (+) (eval x) (eval y)
    eval (Minus x y) = combine (-) (eval x) (eval y)
    eval (Times x y) = combine (*) (eval x) (eval y)
    eval (Divide x y) = combine div (eval x) (eval y)
    eval (Deref x) = eval x

evalB :: Map String ConstData -> BExpr -> ConstData
evalB input = eval
    where
    combineB :: (Bool -> Bool -> Bool) -> ConstData -> ConstData -> ConstData
    combineB op (Bool x) (Bool y) = Bool (op x y)
    combineB _ _ _ = Top
    combineI :: (Int -> Int -> Bool) -> ConstData -> ConstData -> ConstData
    combineI op (Nat x) (Nat y) = Bool (op x y)
    combineI _ _ _ = Top
    eval :: BExpr -> ConstData
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
    eval (Not x) = notConstData (eval x)
    notConstData :: ConstData -> ConstData
    notConstData (Bool x) = Bool (not x)
    notConstData x = x
