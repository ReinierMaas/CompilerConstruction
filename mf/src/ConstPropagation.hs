module ConstPropagation (
    extremalValue,
    merge,
    unaryTransfer,
    binaryTransfer,
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

unaryTransfer :: ProcOrStat -> [(Context, Map String Result)] -> [(Context, Map String Result)]
unaryTransfer (P _) = id
unaryTransfer (S s) = transferStat s

binaryTransfer :: (Int, Int) -> Map Int ProcOrStat -> Map Int [(Context, Map String Result)] -> [(Context, Map String Result)] -> [(Context, Map String Result)]
binaryTransfer (l, l') nodes analysis = case nodes Map.! l of
    (P p@(Proc' _ exit _ _ _ _)) -> if exit == l
                                    then let call = nodes Map.! l'
                                             a = analysis Map.! (getCallLabel call)
                                         in  transferProc p a
                                    else id
    (S s)                        -> transferStat s
    where
    getCallLabel :: ProcOrStat -> Int
    getCallLabel (S (Call' call _ _ _ _)) = call
    getCallLabel x = error $ "Called getCallLabel with wrong parameter: " ++ show x

-- Only called for edges between a proc exit node and a call end node
transferProc :: Proc' -> [(Context, Map String Result)] -> [(Context, Map String Result)] -> [(Context, Map String Result)]
transferProc (Proc' entry ret name inp out stat) callAnalysis = id

transferStat :: Stat' -> [(Context, Map String Result)] -> [(Context, Map String Result)]
transferStat (Call' call exit name inp out) input = undefined
transferStat x input = lift (transferStat' x) input
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
