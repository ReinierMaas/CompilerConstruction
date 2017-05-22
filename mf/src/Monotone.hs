module Monotone where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

mfp :: Eq a => [Int] -> a -> [(Int, Int)] -> (Int -> a -> a) -> (a -> a -> a) -> (Map Int a, Map Int a)
mfp extremalLabels extremalValue transitions transfer join =
    let a = Map.fromList $ map (\l -> (l, extremalValue)) extremalLabels
        openMfp = iterate transitions a
        closedMfp = Map.mapWithKey transfer openMfp
    in  (openMfp, closedMfp)
    where iterate [] x = x
          iterate ((l, l') : ls) x = let trans = transfer l (x Map.! l)
                                         joined = join (x Map.! l') trans
                                     in  if x Map.! l' == joined
                                         then iterate ls x
                                         else iterate (edgesFrom l' transitions ++ ls) (Map.insert l' joined x)

edgesFrom :: Int -> [(Int, Int)] -> [(Int, Int)]
edgesFrom x = filter (\(l, _) -> l == x)
