module Monotone where

import Control.Applicative ((<|>))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import AttributeGrammar (ProcOrStat)

mergeStuff :: (a -> a -> a) -> Maybe a -> Maybe a -> Maybe a
mergeStuff merge (Just x) (Just y) = Just $ merge x y
mergeStuff _ x y = x <|> y

mfp :: Eq a => Map Int ProcOrStat -> [Int] -> a -> [(Int, Int)] -> (ProcOrStat -> Maybe a -> Maybe a) -> (a -> a -> a) -> Map Int (Maybe a, Maybe a)
mfp nodes extremalLabels extremalValue transitions transfer merge =
    let nothings = fmap (const Nothing) nodes
        justs = Map.fromList $ map (\l -> (l, Just extremalValue)) extremalLabels
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
