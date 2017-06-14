module FunFlow.TypeSystem where

import FunFlow.Ast
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
    applyOnType ns (TypeInt) = TypeInt
    applyOnType ns (TypeBool) = TypeBool
    applyOnType ns (TypeFn t1 pi t2) = TypeFn (applyOnType ns t1) pi (applyOnType ns t2)
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

{- Constraints and annotations -}
type Annotation = Set Int

{- Types and schemes -}
data Type = TypeInt
          | TypeBool
          | TypeFn Type Int Type
          | Alpha Int
          deriving (Eq, Ord)

data TypeScheme = TypeScheme (Set Int) Type

instance Show TypeScheme where
    show (TypeScheme as t) = "forall " ++ intercalate " " (map show (Set.toList as)) ++ ". " ++ show t

instance Show Type where
    show (TypeInt) = "Int"
    show (TypeBool) = "Bool"
    show (TypeFn t1 pi t2) = show t1 ++ " " ++ show pi ++ "-> " ++ show t2
    show (Alpha a) = "a" ++ show a

{- Generalization -}
generalize :: TypeEnv -> Type -> TypeScheme
generalize env t = TypeScheme (Set.difference (freeInType t) (freeInEnv env)) t
    where
    freeInType :: Type -> Set Int
    freeInType (TypeFn t1 _ t2) = Set.union (freeInType t1) (freeInType t2)
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
    inst d TypeInt = TypeInt
    inst d TypeBool = TypeBool
    inst d alpha@(Alpha a) = case Map.lookup a d of
                                        Nothing -> alpha
                                        Just b -> b
    inst d (TypeFn a pi b) = TypeFn (inst d a) pi (inst d b)

{- Unification -}
unify :: Type -> Type -> TypeSubstitution
unify t1 t2 = fromEither $ tryUnify t1 t2
    where
    fromEither (Left s) = error s
    fromEither (Right r) = r

tryUnify :: Type -> Type -> Either String TypeSubstitution
tryUnify TypeInt TypeInt = Right idSub
tryUnify TypeBool TypeBool = Right idSub
tryUnify (Alpha x) (Alpha y) = Right $ substitute x (Alpha y)
tryUnify (TypeFn t1 _ t2) (TypeFn t3 _ t4) = do
    subs1 <- tryUnify t1 t3
    subs2 <- tryUnify (subs1 -$- t2) (subs1 -$- t4)
    Right $ subs2 -.- subs1
tryUnify (Alpha x) t = if not (x `isFreeIn` t)
                       then Right (substitute x t)
                       else unifyFailure (Alpha x) t
tryUnify t a@(Alpha x) = tryUnify a t
tryUnify t1 t2 = unifyFailure t1 t2

unifyFailure t1 t2 = Left $ "Unable to unify " ++ show t1 ++ " and " ++ show t2

isFreeIn :: Int -> Type -> Bool
isFreeIn x (TypeFn t1 _ t2) = isFreeIn x t1 || isFreeIn x t2
isFreeIn x (Alpha y) = x == y
isFreeIn _ _ = False

{- W algorithm for Hilney Miler type inference -}
fresh :: State Int Type
fresh = do
    x <- get
    put $ x + 1
    return $ Alpha x

freshInt :: State Int Int
freshInt = do
    x <- get
    put $ x + 1
    return $ x

w :: TypeEnv -> Expr -> State Int (Type, TypeSubstitution)
w env (Int _) =  do
    return $ debug $ (TypeInt, idSub)
w env (Bool _) =  do
    return $ debug $ (TypeBool, idSub)
w env (Var x) = case envLookup x env of
                    Just ts -> do
                        t <- instantiate ts
                        return $ debug $ (t, idSub)
                    Nothing -> error $ "x : " ++ show x ++ " not found in environment"  -- See slides on page 24. Unclear how to implement.
w env (Fn pi x t1) = do
    a1 <- fresh
    (t2, subs) <- w (envAppend x (TypeScheme Set.empty a1) env) t1
    return $ debug $ (TypeFn (subs -$- a1) pi t2, subs)
w env (Fun pi f x t1) = do
    a1 <- fresh
    a2 <- fresh
    (t2, subs1) <- w (envAppend' [(f, TypeScheme Set.empty (TypeFn a1 pi a2)), (x, TypeScheme Set.empty a1)] env) t1
    let subs2 = unify t2 (subs1 -$- a2)
    return $ debug $ (TypeFn (subs2 -$- (subs1 -$- a1)) pi (subs2 -$- t2), subs2 -.- subs1)
w env (App term1 term2) = do
    (t1, subs1) <- w env term1
    (t2, subs2) <- w (envSubstitute subs1 env) term2
    a <- fresh
    pi <- freshInt
    let subs3 = unify (subs2 -$- t1) (TypeFn t2 pi a)
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
    (t1, subs1) <- w env term1 -- term1: 42 :: Int
    let env1 = envSubstitute subs1 env
    let env2 = debug $ envAppend x (generalize env1 t1) env1
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
opTypes op | op `elem` [Add, Sub, Mul, Div] = (TypeInt, TypeInt, TypeInt)
           | otherwise = error "This is impossible"

debug :: Show a => a -> a
debug = id --Debug.traceShowId