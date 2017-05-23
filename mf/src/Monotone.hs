module Monotone where

import Control.Applicative ((<|>))
import Data.Maybe (fromJust, fromMaybe, isNothing, maybe)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import AttributeGrammar

joinStuff :: (a -> a -> a) -> Maybe a -> Maybe a -> Maybe a
joinStuff join (Just x) (Just y) = Just $ join x y
joinStuff _ x y = x <|> y

mfp :: Eq a => Map Int Stat' -> [Int] -> a -> [(Int, Int)] -> (Stat' -> Maybe a -> Maybe a) -> (a -> a -> a) -> (Map Int (Maybe a), Map Int (Maybe a))
mfp nodes extremalLabels extremalValue transitions transfer join =
    let a = Map.fromList $ map (\l -> (l, Just extremalValue)) extremalLabels
        openMfp = iterate' nodes transitions transfer join transitions a
        closedMfp = Map.mapWithKey (\l v -> transfer (nodes Map.! l) v) openMfp
    in  (openMfp, closedMfp)

iterate' :: Eq a => Map Int Stat' -> [(Int, Int)] -> (Stat' -> Maybe a -> Maybe a) -> (a -> a -> a) -> [(Int, Int)] -> Map Int (Maybe a) -> Map Int (Maybe a)
iterate' nodes transitions transfer join [] a = a
iterate' nodes transitions transfer join ((l, l') : ls) a = let trans = transfer (nodes Map.! l) (joinMaybe (Map.lookup l a))
                                                                joined = joinStuff join trans (joinMaybe (Map.lookup l' a))
                                                            in  if (joinMaybe (Map.lookup l' a)) == joined
                                                                then iterate' nodes transitions transfer join ls a
                                                                else iterate' nodes transitions transfer join (edgesFrom l' transitions ++ ls) (Map.insert l' joined a)

joinMaybe :: Maybe (Maybe a) -> Maybe a
joinMaybe (Just x) = x
joinMaybe Nothing = Nothing

data ConstData = Top
               | Nat Int
               | Bool Bool
               deriving (Eq, Show)

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

constJoin :: Map String ConstData -> Map String ConstData -> Map String ConstData
constJoin = Map.unionWith joinSingle
    where
    joinSingle :: ConstData -> ConstData -> ConstData
    joinSingle x y | x == y = x
                   | otherwise = Top



edgesFrom :: Int -> [(Int, Int)] -> [(Int, Int)]
edgesFrom x = filter (\(l, _) -> l == x)
