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

transfer :: Stat' -> Maybe (Set String) -> Maybe (Set String)
transfer stat = fmap (\input -> (input Set.\\ kill stat) `Set.union` (gen stat Set.\\ kill stat))

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
gen (Seq' _ _) = error "Called gen on Seq"
gen (Call' _ _ name _ _) = error "Called gen on Call"

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
