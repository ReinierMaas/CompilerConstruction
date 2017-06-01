module Monotone where

import Control.Applicative ((<|>))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import AttributeGrammar (ProcOrStat(..), Stat'(Call'), Proc'(..))

type Context = [Int]

emptyContext = []

maxContextDepth = 1 -- 0 no context, 1 callsite information, 2 two levels callsite information, ...

mergeStuff :: (a -> a -> a) -> [(Context, a)] -> [(Context, a)] -> [(Context, a)]
mergeStuff merge xs ys = Map.toList $ Map.unionWith merge (Map.fromList xs) (Map.fromList ys)

type UnaryTransfer a = ProcOrStat -> [(Context, a)] -> [(Context, a)]
type BinaryTransfer a = (Int, Int) -> Map Int ProcOrStat -> Map Int [(Context, a)] -> [(Context, a)] -> [(Context, a)]

liftTransfer :: (a -> a) -> [(Context, a)] -> [(Context, a)]
liftTransfer f = map (\(ctx, m) -> (ctx, f m))

mfp :: Eq a => Map Int ProcOrStat -- nodes
            -> [Int]              -- extremal labels
            -> a                  -- extremal value
            -> [(Int, Int)]       -- transitions
            -> UnaryTransfer a    -- unary transfer function
            -> BinaryTransfer a   -- binary transfer function
            -> (a -> a -> a)      -- merge function
            -> Map Int ([(Context, a)], [(Context, a)]) -- resulting analysis
mfp nodes extremalLabels extremalValue transitions transferUnary transferBinary merge =
    let nothings = fmap (const []) nodes
        justs = Map.fromList $ map (\l -> (l, [(emptyContext, extremalValue)])) extremalLabels
        a = justs `Map.union` nothings -- Note: in case of duplicated keys, justs is preferred
        openMfp = fixpoint a transitions
    in  Map.mapWithKey (\l v -> (v, transferUnary (nodes Map.! l) v)) openMfp
    where
    fixpoint a [] = a
    fixpoint a ((l, l') : ls) = let node = (nodes Map.! l)
                                    trans = transferBinary (l, l') nodes a (a Map.! l)
                                    merged = mergeStuff merge trans (a Map.! l')
                                in  if (a Map.! l') == merged
                                    then fixpoint a ls
                                    else fixpoint (Map.insert l' merged a) (edgesFrom l' transitions ++ ls)
    edgesFrom :: Int -> [(Int, Int)] -> [(Int, Int)]
    edgesFrom x = filter (\(l, _) -> l == x)
