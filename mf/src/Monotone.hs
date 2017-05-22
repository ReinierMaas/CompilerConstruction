module Monotone where

import Control.Applicative ((<|>))
import Data.Maybe (fromMaybe, maybe)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import AttributeGrammar

joinStuff :: (a -> a -> a) -> Maybe a -> Maybe a -> Maybe a
joinStuff join (Just x) (Just y) = Just $ join x y
joinStuff _ x y = x <|> y

mfp :: Eq a => Map Int Stat' -> [Int] -> a -> [(Int, Int)] -> (Stat' -> Maybe a -> Maybe a) -> (a -> a -> a) -> (Map Int a, Map Int a)
mfp nodes extremalLabels extremalValue transitions transfer join =
    let a = Map.fromList $ map (\l -> (l, extremalValue)) extremalLabels
        openMfp = iterate transitions a
        openMfp' = Map.mapKeys (nodes Map.!) (Map.mapWithKey (\k v -> (k, v)) openMfp)
        closedMfp = Map.mapWithKey (\k (l, v) -> (l, transfer k v)) openMfp'
        closedMfp' = Map.fromList $ map snd $ Map.toList closedMfp
    in  (openMfp, closedMfp')
    where
    --iterate :: [(Int, Int)] -> Maybe (Map Int a) -> Maybe (Map Int a)
    iterate [] x = x
    iterate ((l, l') : ls) x = let trans = transfer (nodes Map.! l) (Map.lookup l x)
                                   joined = joinStuff join (Map.lookup l' x) trans
                               in  if Map.lookup l' x == joined
                                   then iterate ls x
                                   else iterate (edgesFrom l' transitions ++ ls) (Map.insert l' joined x)

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
    eval (BConst v) = Just v
    eval (BVar n) =  Map.lookup n input
    eval _ = undefined

constJoin :: Map String ConstData -> Map String ConstData -> Map String ConstData
constJoin = Map.unionWith joinSingle
    where
    joinSingle :: ConstData -> ConstData -> ConstData
    joinSingle x y | x == y = x
                   | otherwise = Top



edgesFrom :: Int -> [(Int, Int)] -> [(Int, Int)]
edgesFrom x = filter (\(l, _) -> l == x)
