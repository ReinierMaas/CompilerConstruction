module ConstPropagation (
    extremalValue,
    merge,
    transfer,
    Result(..)
) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Monotone (Context)

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

transfer :: ProcOrStat -> [(Context, Map String Result)] -> [(Context, Map String Result)]
transfer (P p) = transferProc p
transfer (S s) = transferStat s

transferProc :: Proc' -> [(Context, Map String Result)] -> [(Context, Map String Result)]
transferProc (Proc' entry ret name inp out stat) = id

transferStat :: Stat' -> [(Context, Map String Result)] -> [(Context, Map String Result)]
transferStat (Call' _ _ _ _ _) = id
transferStat x = lift $ transferStat' x
    where
    lift :: (Map String Result -> Map String Result) -> [(Context, Map String Result)] -> [(Context, Map String Result)]
    lift f = map (\(ctx, m) -> (ctx, f m))

transferStat' :: Stat' -> Map String Result -> Map String Result
transferStat' (IAssign' _ name val) input = Map.insert name (evalI input val) input
transferStat' (BAssign' l name val) input = Map.insert name (evalB input val) input
transferStat' _ input = input

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
