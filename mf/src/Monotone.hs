module Monotone where

import Control.Applicative ((<|>))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import AttributeGrammar (ProcOrStat)

type Context = [Int]

emptyContext = []

maxContextDepth = 1 -- 0 no context, 1 callsite information, 2 two levels callsite information, ...

mergeStuff :: (a -> a -> a) -> [(Context, a)] -> [(Context, a)] -> [(Context, a)]
mergeStuff merge xs ys = undefined

mfp :: Eq a => Map Int ProcOrStat -> [Int] -> a -> [(Int, Int)] -> (ProcOrStat -> [(Context, a)] -> [(Context, a)]) -> (a -> a -> a) -> Map Int ([(Context, a)], [(Context, a)])
mfp nodes extremalLabels extremalValue transitions transfer merge =
    let nothings = fmap (const []) nodes
        justs = Map.fromList $ map (\l -> (l, [(emptyContext, extremalValue)])) extremalLabels
        a = justs `Map.union` nothings -- Note: in case of duplicated keys, justs is preferred
        openMfp = fixpoint a transitions
    in  Map.mapWithKey (\l v -> (v, transfer (nodes Map.! l) v)) openMfp
    where
    fixpoint a [] = a
    fixpoint a ((l, l') : ls) = let trans = transfer (nodes Map.! l) (a Map.! l)
                                    merged = mergeStuff merge trans (a Map.! l')
                                in  if (a Map.! l') == merged
                                    then fixpoint a ls
                                    else fixpoint (Map.insert l' merged a) (edgesFrom l' transitions ++ ls)
    edgesFrom :: Int -> [(Int, Int)] -> [(Int, Int)]
    edgesFrom x = filter (\(l, _) -> l == x)
