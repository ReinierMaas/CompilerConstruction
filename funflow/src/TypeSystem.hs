module TypeSystem where

import Ast
import Control.Monad.State.Lazy (State, get, put)
import Data.Function (fix)
import Data.List (find, intercalate)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.Maybe (fromJust)
import Data.Either (Either(..))
import qualified Data.Set as Set
import Data.Set (Set)

import qualified Debug.Trace as Debug

{- Environment -}
type TypeEnv = Map Name TypeScheme

envLookup :: Name -> TypeEnv -> Maybe TypeScheme
envLookup = Map.lookup

envAppend :: Name -> TypeScheme -> TypeEnv -> TypeEnv
envAppend = Map.insert

envAppend' :: [(Name, TypeScheme)] -> TypeEnv -> TypeEnv
envAppend' new env = foldr (\(n,ts) acc -> envAppend n ts acc) env (reverse new)

envSubstitute :: TypeSubstitution -> TypeEnv -> TypeEnv
envSubstitute sub env = Map.map applyOnScheme env
    where
    applyOnScheme :: TypeScheme -> TypeScheme
    applyOnScheme (TypeScheme as t) = TypeScheme as (applyOnType (substituteMany as sub) t)
    applyOnType :: TypeSubstitution -> Type -> Type
    applyOnType ns (TypeInteger) = TypeInteger
    applyOnType ns (TypeBool) = TypeBool
    applyOnType ns (TypeFn t1 t2) = TypeFn (applyOnType ns t1) (applyOnType ns t2)
    applyOnType ns a@(Alpha x) = ns -$- a

{- Substitutions -}
newtype TypeSubstitution = TypeSubstitution (Map Int Type) deriving (Show)

(-.-) :: TypeSubstitution -> TypeSubstitution -> TypeSubstitution
(-.-) (TypeSubstitution m1) (TypeSubstitution m2) = TypeSubstitution (Map.union m1 m2)

(-$-) :: TypeSubstitution -> Type -> Type
(-$-) (TypeSubstitution m) (Alpha t) = case Map.lookup t m of
                                            Just t' -> t'
                                            Nothing -> Alpha t
(-$-) _ t = t

idSub :: TypeSubstitution
idSub = TypeSubstitution Map.empty

substituteMany :: Set Int -> TypeSubstitution -> TypeSubstitution
substituteMany as ts = TypeSubstitution (Map.fromSet Alpha as) -.- ts

substitute :: Int -> Type -> TypeSubstitution
substitute x t2 = TypeSubstitution $ Map.singleton x t2

{- Other stuff -}
data Type = TypeInteger
          | TypeBool
          | TypeFn Type Type
          | Alpha Int
          deriving (Eq, Ord)

data TypeScheme = TypeScheme (Set Int) Type

instance Show TypeScheme where
    show (TypeScheme as t) = "forall " ++ intercalate " " (map show (Set.toList as)) ++ ". " ++ show t

instance Show Type where
    show (TypeInteger) = "Int"
    show (TypeBool) = "Bool"
    show (TypeFn t1 t2) = show t1 ++ " -> " ++ show t2
    show (Alpha a) = "a" ++ show a

{- Generalization -}
generalize :: TypeEnv -> Type -> TypeScheme
generalize env t = TypeScheme (Set.difference (freeInType t) (freeInEnv env)) t
    where
    freeInType :: Type -> Set Int
    freeInType (TypeFn t1 t2) = Set.union (freeInType t1) (freeInType t2)
    freeInType (Alpha x) = Set.singleton x
    freeInType _ = Set.empty
    freeInScheme :: TypeScheme -> Set Int
    freeInScheme (TypeScheme as t1) = Set.difference (freeInType t1) as
    freeInEnv :: TypeEnv -> Set Int
    freeInEnv = foldr (\ts acc -> Set.union acc $ freeInScheme ts) Set.empty

{- Instantiation -}
instantiate :: TypeScheme -> State Int Type
instantiate (TypeScheme as t) = inst <$> dict <*> pure t
    where
    dict :: State Int (Map Int Type)
    dict = do
        freshs <- sequence $ take (Set.size as) $ repeat fresh
        return $ Map.fromList $ zip (Set.toList as) freshs
    inst :: Map Int Type -> Type -> Type
    inst d TypeInteger = TypeInteger
    inst d TypeBool = TypeBool
    inst d alpha@(Alpha a) = case Map.lookup a d of
                                        Nothing -> alpha
                                        Just b -> b
    inst d (TypeFn a b) = TypeFn (inst d a) (inst d b)

{- Unification -}
unify :: Type -> Type -> TypeSubstitution
unify TypeInteger TypeInteger = idSub
unify TypeBool TypeBool = idSub
unify (Alpha x) (Alpha y) = substitute x (Alpha y)
unify (TypeFn t1 t2) (TypeFn t3 t4) = subs2 -.- subs1
    where
    subs1 = unify t1 t3
    subs2 = unify (subs1 -$- t2) (subs1 -$- t4)
unify (Alpha x) t = if not (x `isFreeIn` t) then substitute x t else unifyFailure (Alpha x) t
unify t a@(Alpha x) = unify a t
unify t1 t2 = unifyFailure t1 t2

unifyFailure t1 t2 = error $ "Unable to unify " ++ show t1 ++ " and " ++ show t2

isFreeIn :: Int -> Type -> Bool
isFreeIn x (TypeFn t1 t2) = isFreeIn x t1 || isFreeIn x t2
isFreeIn x (Alpha y) = x == y
isFreeIn _ _ = False

{- W algorithm for Hilney Miler type inference -}
fresh :: State Int Type
fresh = do
    x <- get
    put $ x + 1
    return $ Alpha x

w :: TypeEnv -> Expr -> State Int (Type, TypeSubstitution)
w env (Integer _) =  do
    return $ debug $ (TypeInteger, idSub)
w env (Bool _) =  do
    return $ debug $ (TypeBool, idSub)
w env (Var x) = case envLookup x env of
                    Just ts -> do
                        t <- instantiate ts
                        return $ debug $ (t, idSub)
                    Nothing -> error $ "x : " ++ show x ++ " not found in environment"  -- See slides on page 24. Unclear how to implement.
w env (Fn p x t1) = do
    a1 <- fresh
    (t2, subs) <- w (envAppend x (TypeScheme Set.empty a1) env) t1
    return $ debug $ (TypeFn (subs -$- a1) t2, subs)
w env (Fun p f x t1) = do
    a1 <- fresh
    a2 <- fresh
    (t2, subs1) <- w (envAppend' [(f, TypeScheme Set.empty (TypeFn a1 a2)), (x, TypeScheme Set.empty a1)] env) t1
    let subs2 = unify t2 (subs1 -$- a2)
    return $ debug $ (TypeFn (subs2 -$- (subs1 -$- a1)) (subs2 -$- t2), subs2 -.- subs1)
w env (App term1 term2) = do
    (t1, subs1) <- w env term1
    (t2, subs2) <- w (envSubstitute subs1 env) term2
    a <- fresh
    let subs3 = unify (subs2 -$- t1) (TypeFn t2 a)
    return $ debug $ (subs3 -$- a, subs3 -.- subs2 -.- subs1)
w env (ITE term1 term2 term3) = do
    (t1, subs1) <- w env term1
    let env1 = envSubstitute subs1 env
    (t2, subs2) <- w env1 term2
    let env2 = envSubstitute subs2 env1
    (t3, subs3) <- w env2 term3
    let subs4 = unify (subs3 -$- (subs2 -$- t1)) TypeBool
    let subs5 = unify (subs4 -$- (subs3 -$- t2)) (subs4 -$- t3)
    return $ debug $ (subs5 -$- (subs4 -$- t3), subs5 -.- subs4 -.- subs3 -.- subs2 -.- subs1)
w env (Let x term1 term2) = do
    (t1, subs1) <- w env term1 -- term1: 42 :: Integer
    let env1 = envSubstitute subs1 env
    let env2 = envAppend x (generalize env1 t1) env1
    z <- Debug.traceShowM env2
    (t2, subs2) <- w env2 term2 -- term2: ITE
    return $ debug $ (t2, subs2 -.- subs1)
w env (Oper op term1 term2) = do
    (t1, subs1) <- w env term1
    let env1 = envSubstitute subs1 env
    (t2, subs2) <- w env1 term2
    let (param1, param2, ret) = opTypes op
    let subs3 = unify (subs2 -$- t1) param1
    let subs4 = unify (subs3 -$- t2) param2
    return $ debug $ (ret, subs4 -.- subs3 -.- subs2 -.- subs1)

opTypes :: Op -> (Type, Type, Type)
opTypes op | op `elem` [Add, Sub, Mul, Div] = (TypeInteger, TypeInteger, TypeInteger)
           | otherwise = error "This is impossible"

debug :: Show a => a -> a
debug = Debug.traceShowId