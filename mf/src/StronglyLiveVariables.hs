module StronglyLiveVariables (
    extremalValue,
    merge,
    unaryTransfer,
    binaryTransfer
) where

import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)

import AttributeGrammar
import Monotone (Context, liftTransfer)

extremalValue :: Set String
extremalValue = Set.empty

merge :: Set String -> Set String -> Set String
merge = Set.union

unaryTransfer :: ProcOrStat -> [(Context, Set String)] -> [(Context, Set String)]
unaryTransfer (P _) = id -- Ignore procedures
unaryTransfer (S s) = liftTransfer $ transfer s

binaryTransfer :: (Int, Int) -> Map Int ProcOrStat -> Map Int [(Context, Set String)] ->  [(Context, Set String)] -> [(Context, Set String)]
binaryTransfer (l, _) nodes _ = unaryTransfer $ nodes Map.! l

transfer :: Stat' -> Set String -> Set String
transfer stat input = (input Set.\\ kill stat) `Set.union` (gen stat Set.\\ kill stat)

kill :: Stat' -> Set String
kill (IAssign' _ name _) = Set.singleton name
kill (BAssign' _ name _) = Set.singleton name
kill (Malloc'  _ name _) = Set.singleton name
kill _ = Set.empty

gen :: Stat' -> Set String
gen (Skip' _) = Set.empty
gen (IfThenElse' _ cond _ _) = freeVarsB cond
gen (While' _ cond _) = freeVarsB cond
gen (IAssign' _ _ val) = freeVarsI val
gen (BAssign' _ _ val) = freeVarsB val
gen (Malloc' _ name size) = Set.empty
gen (Free' _ _) = Set.empty
gen (RefAssign' _ _ val) = freeVarsI val
gen (Continue' _) = Set.empty
gen (Break' _) = Set.empty
gen (Call' _ _ name _ _) = Set.empty
gen (Seq' _ _) = error "Called gen on Seq"

freeVarsI :: IExpr -> Set String
freeVarsI (IConst v) = Set.empty
freeVarsI (Var n) = Set.singleton n
freeVarsI (Plus x y) = Set.union (freeVarsI x) (freeVarsI y)
freeVarsI (Minus x y) = Set.union (freeVarsI x) (freeVarsI y)
freeVarsI (Times x y) = Set.union (freeVarsI x) (freeVarsI y)
freeVarsI (Divide x y) = Set.union (freeVarsI x) (freeVarsI y)
freeVarsI (Deref x) = freeVarsI x

freeVarsB :: BExpr -> Set String
freeVarsB (BConst v) = Set.empty
freeVarsB (BVar n) = Set.singleton n
freeVarsB (LessThan x y) = Set.union (freeVarsI x) (freeVarsI y)
freeVarsB (GreaterThan x y) = Set.union (freeVarsI x) (freeVarsI y)
freeVarsB (LessEqual x y) = Set.union (freeVarsI x) (freeVarsI y)
freeVarsB (GreaterEqual x y) = Set.union (freeVarsI x) (freeVarsI y)
freeVarsB (IEqual x y) = Set.union (freeVarsI x) (freeVarsI y)
freeVarsB (BEqual x y) = Set.union (freeVarsB x) (freeVarsB y)
freeVarsB (And x y) = Set.union (freeVarsB x) (freeVarsB y)
freeVarsB (Or x y) = Set.union (freeVarsB x) (freeVarsB y)
freeVarsB (Not x) = freeVarsB x
