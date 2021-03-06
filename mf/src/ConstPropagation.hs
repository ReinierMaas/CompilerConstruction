module ConstPropagation (
    runAnalysis,
    Result(..)
) where

import Data.Graph.Inductive.Graph (labNodes, edges)
import Data.Graph.Inductive.PatriciaTree (Gr)
import Data.List (find)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust, fromMaybe)
import Monotone (Context, liftTransfer, mfp)

import AttributeGrammar

-- The Const Propagation analysis
runAnalysis :: Int -> Gr ProcOrStat () -> Int ->Map Int ([(Context, Map String Result)], [(Context, Map String Result)])
runAnalysis maxDepth graph entryLabel = mfp (Map.fromList (labNodes graph)) [entryLabel] extremalValue (edges graph) unaryTransfer (binaryTransfer maxDepth) merge

data Result = Top
            | Nat Int
            | Bool Bool
            deriving (Eq, Show)

-- Internal functions
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
unaryTransfer (S (Call' _ _ _ _ _)) = id
unaryTransfer (S s) = transferStat s

binaryTransfer :: Int -> (Int, Int) -> Map Int ProcOrStat -> Map Int [(Context, Map String Result)] -> [(Context, Map String Result)] -> [(Context, Map String Result)]
binaryTransfer maxDepth (l, l') nodes analysis = case nodes Map.! l of
    (P p@(Proc' _ exit _ _ _ _))    -> if exit == l
                                       then let call = nodes Map.! l'
                                                a = analysis Map.! (getCallLabel call)
                                            in  transferProc maxDepth p (getStat call) a
                                       else id
    (S s@(Call' lc lc' name _ _))   -> let p = getProc $ fromJust $ find (isProc name) $ map snd $ Map.toList nodes in
                                       if l == lc && l' == lc' -- Edge between call entry and call exit at the call site
                                       then transferProc maxDepth p s (analysis Map.! lc) -- Join the analysis from the procedure with the current analysis
                                       else if l == lc' -- Edge from call exit to something
                                            then id
                                            else transferCall maxDepth s p -- Edge from call entry to proc entry
    (S s)                           -> transferStat s
    where
    getCallLabel :: ProcOrStat -> Int
    getCallLabel (S (Call' call _ _ _ _)) = call
    getCallLabel x = error $ "Called getCallLabel with wrong parameter: " ++ show x

    getStat :: ProcOrStat -> Stat'
    getStat (S call) = call
    getStat (P p) = error $ "Called getStat on Proc': " ++ show p

    getProc :: ProcOrStat -> Proc'
    getProp (S s) = error $ "Called getProc on Stat': " ++ show s
    getProc (P proc) = proc

    isProc :: String -> ProcOrStat -> Bool
    isProc name (P (Proc' _ _ nameProc _ _ _)) = name == nameProc
    isProc _ _ = False

newContext :: Int -> Int -> Context -> Context
newContext maxDepth callLabel ctx = take maxDepth (callLabel : ctx)

-- Only called for edges between a proc exit node and a call end node
transferProc :: Int -> Proc' -> Stat' -> [(Context, Map String Result)] -> [(Context, Map String Result)] -> [(Context, Map String Result)]
transferProc maxDepth (Proc' entryLabel returnLabel nameProc inputs result _) (Call' callLabel exitLabel nameCall params output) callAnalysis input
    = map (\(ctx, val) -> (ctx, combine (returnResult ctx) val)) callAnalysis
    where
    returnResult :: Context -> Maybe Result
    returnResult ctx = Map.lookup (newContext maxDepth callLabel ctx) (Map.fromList input) >>= Map.lookup result
    combine :: Maybe Result -> Map String Result -> Map String Result
    combine (Just ret) = Map.insert output ret
    combine Nothing = Map.delete result
    isInit :: Eq a => [a] -> [a] -> Bool
    isInit [] _ = True
    isInit (x:xs) (y:ys) = x == y && isInit xs ys

-- Only called for calls with the corresponding procedure provided (for edges between call entry and proc entry)
transferCall :: Int -> Stat' -> Proc' -> [(Context, Map String Result)] -> [(Context, Map String Result)]
transferCall maxDepth (Call' callLabel exitLabel nameCall params output) (Proc' entryLabel returnLabel nameProc inputs result _) input
    = Map.toList $ Map.fromListWith merge $ map (\(ctx, vals) -> (newContext maxDepth callLabel ctx, valsToInput vals)) input
    where
    valsToInput :: Map String Result -> Map String Result
    valsToInput vals = Map.fromList $ map (\(i, p) -> (i, eval vals p)) (zip inputs params)
transferCall _ _ _ _ = error "Called transferCall without a Call"

transferStat :: Stat' -> [(Context, Map String Result)] -> [(Context, Map String Result)]
transferStat (Call' _ _ _ _ _) _ = error "Called transferStat with a Call"
transferStat s input = liftTransfer (transferStat' s) input
    where
    transferStat' :: Stat' -> Map String Result -> Map String Result
    transferStat' (IAssign' _ name val) input = Map.insert name (evalI input val) input
    transferStat' (BAssign' l name val) input = Map.insert name (evalB input val) input
    transferStat' _ input = input

eval :: Map String Result -> Expr -> Result
eval input (I i) = evalI input i
eval input (B b) = evalB input b

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
