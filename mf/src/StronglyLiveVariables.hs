module StronglyLiveVariables (
    extremalValue,
    merge,
    transfer
) where

import Data.Set (Set)
import qualified Data.Set as Set
import Data.Maybe (fromMaybe)

import AttributeGrammar

extremalValue :: Set String
extremalValue = Set.empty

merge :: Set String -> Set String -> Set String
merge = Set.union

transfer :: ProcOrStat -> Maybe (Set String) -> Maybe (Set String)
transfer stat = fmap (\input -> (input Set.\\ kill stat) `Set.union` (gen stat Set.\\ kill stat))

kill :: ProcOrStat -> Set String
kill (S (Call' _ _ _ _ out)) = Set.singleton out
kill (S (IAssign' _ name _)) = Set.singleton name
kill (S (BAssign' _ name _)) = Set.singleton name
kill (S (Malloc'  _ name _)) = Set.singleton name
kill _ = Set.empty

gen :: ProcOrStat -> Set String
gen (S (Skip' _)) = Set.empty
gen (S (IfThenElse' _ cond _ _)) = freeVarsB cond
gen (S (While' _ cond _)) = freeVarsB cond
gen (S (Call' _ _ name params _)) = Set.unions $ map freeVars params
gen (S (IAssign' _ _ val)) = freeVarsI val
gen (S (BAssign' _ _ val)) = freeVarsB val
gen (S (Malloc' _ name size)) = Set.empty
gen (S (Free' _ _)) = Set.empty
gen (S (RefAssign' _ _ val)) = freeVarsI val
gen (S (Continue' _)) = Set.empty
gen (S (Break' _)) = Set.empty
gen (S (Seq' _ _)) = error "Called gen on Seq"
gen (P (Proc' _ _ _ _ _ _)) = Set.empty

freeVars :: Expr -> Set String
freeVars (I i) = freeVarsI i
freeVars (B b) = freeVarsB b

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
