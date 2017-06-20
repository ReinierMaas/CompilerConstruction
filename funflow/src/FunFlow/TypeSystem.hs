module FunFlow.TypeSystem where

import FunFlow.Ast
import Control.Monad (foldM)
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
    applyOnType ns (TypeGeneral pi types) = TypeGeneral pi $ map (applyOnType ns) types
    applyOnType ns a@(Alpha x) = ns -$- a

{- Substitutions -}
data TypeSubstitution = TypeSubstitution (Map Int Type, Map AnnVar AnnVar) deriving (Show)

(-.-) :: TypeSubstitution -> TypeSubstitution -> TypeSubstitution
(-.-) last@(TypeSubstitution (m1, c1)) (TypeSubstitution (m2, c2)) =
    TypeSubstitution ( Map.union m1 (Map.map (last -$-) m2)
                     , Map.union c1 (Map.map (last -$$-) c2))

(-$-) :: TypeSubstitution -> Type -> Type
(-$-) (TypeSubstitution (m, _)) (Alpha t) = case Map.lookup t m of
                                                Just t' -> t'
                                                Nothing -> Alpha t
(-$-) ts (TypeFn t1 b t2) =
    let t1' = ts -$- t1
        t2' = ts -$- t2
        b' = ts -$$- b
    in TypeFn t1' b' t2'
(-$-) ts@(TypeSubstitution (m, c)) (TypeGeneral b t) =
    let t' = map (ts -$-) t
        b' = ts -$$- b
    in  TypeGeneral b' t'
(-$-) _ t = t

(-$$-) :: TypeSubstitution -> AnnVar -> AnnVar
(-$$-) (TypeSubstitution (_, c)) b = case Map.lookup b c of
                                        Just b' -> b'
                                        Nothing -> b

idSub :: TypeSubstitution
idSub = TypeSubstitution (Map.empty, Map.empty)

substituteMany :: Set Int -> TypeSubstitution -> TypeSubstitution
substituteMany as ts = TypeSubstitution (Map.fromSet Alpha as, Map.empty) -.- ts

substitute :: Int -> Type -> TypeSubstitution
substitute x t2 = TypeSubstitution (Map.singleton x t2, Map.empty)

bSubstitute :: AnnVar -> AnnVar -> TypeSubstitution
bSubstitute b1 b2 = TypeSubstitution (Map.empty, Map.singleton b1 b2)

{- Constraints and annotations -}
newtype AnnVar = AnnVar Int deriving (Eq, Ord)
instance Show AnnVar where
    show (AnnVar x) = show x

data Constraints = Constraints (Map AnnVar (Set AnnVar))

instance Show Constraints where
    show (Constraints c) = show $ Map.toList $ Map.map Set.toList c

cEmpty :: Constraints
cEmpty = Constraints Map.empty

cUnion :: Constraints -> Constraints -> Constraints
cUnion (Constraints c1) (Constraints c2) = Constraints $ Map.unionWith (Set.union) c1 c2

cSuperset :: AnnVar -> AnnVar -> Constraints
cSuperset av x = Constraints $ Map.singleton av (Set.singleton x)

conSubstitute :: TypeSubstitution -> Constraints -> Constraints
conSubstitute ts (Constraints c) = Constraints $ Map.mapKeysWith (Set.union) (ts -$$-) c

{- Types and schemes -}
data Type = TypeInt
          | TypeBool
          | TypeFn Type AnnVar Type
          | TypeGeneral AnnVar [Type]
          | Alpha Int
          deriving (Eq, Ord)

data TypeScheme = TypeScheme (Set Int) Type

instance Show TypeScheme where
    show (TypeScheme as t) = "forall " ++ intercalate " " (map show (Set.toList as)) ++ ". " ++ show t

instance Show Type where
    show (TypeInt) = "Int"
    show (TypeBool) = "Bool"
    show (TypeFn t1 pi t2) = show t1 ++ " " ++ show pi ++ "-> " ++ show t2
    show (TypeGeneral pi types) = "TypeGeneral (" ++ show pi ++ ") " ++ show types
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
    inst d (TypeGeneral pi types) = TypeGeneral pi $ map (inst d) types

{- Unification -}
unify :: Type -> Type -> TypeSubstitution
unify t1 t2 = fromEither $ tryUnify t1 t2
    where
    fromEither (Left s) = error s
    fromEither (Right r) = r

tryUnify :: Type -> Type -> Either String TypeSubstitution
tryUnify TypeInt TypeInt = Right idSub
tryUnify TypeBool TypeBool = Right idSub
tryUnify (TypeGeneral b1 types1) (TypeGeneral b2 types2) = do
    let subs0 = bSubstitute b1 b2
    if length types1 == length types2
    then foldr f (Right subs0) $ zip types1 types2
    else unifyFailure types1 types2
    where
        f :: (Type, Type) -> Either String TypeSubstitution -> Either String TypeSubstitution
        f (t1, t2) (Right prevSub) = (-.- prevSub) <$> tryUnify (prevSub -$- t1) (prevSub -$- t2)
        f _ l@(Left _) = l
tryUnify (Alpha x) (Alpha y) = Right $ substitute x (Alpha y)
tryUnify (TypeFn t1 b1 t2) (TypeFn t3 b2 t4) = do
    let subs0 = bSubstitute b1 b2
    subs1 <- tryUnify (subs0 -$- t1) (subs0 -$- t3)
    subs2 <- tryUnify (subs1 -.- subs0 -$- t2) (subs1 -.- subs0 -$- t4)
    Right $ subs2 -.- subs1 -.- subs0
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
fresh = Alpha <$> freshInt

freshInt :: State Int Int
freshInt = do
    x <- get
    put $ x + 1
    return $ x

freshAnnVar :: State Int AnnVar
freshAnnVar = AnnVar <$> freshInt

w :: TypeEnv -> Expr -> State Int (Type, TypeSubstitution, Constraints)
w env (Int _) = return $ debug $ (TypeInt, idSub, cEmpty)
w env (Bool _) = return $ debug $ (TypeBool, idSub, cEmpty)
w env (Var x) = case envLookup x env of
                    Just ts -> do
                        t <- instantiate ts
                        return $ debug $ (t, idSub, cEmpty)
                    Nothing -> error $ "x : " ++ show x ++ " not found in environment"
w env (Pair pi term1 term2) = do
    (t1, subs1, c1) <- w env term1

    (t2, subs2, c2) <- w (envSubstitute subs1 env) term2

    b <- freshAnnVar
    return $ debug $ ( TypeGeneral b [subs2 -$- t1, t2]
                     , subs2 -.- subs1
                     , cUnion (cUnion c2 ((conSubstitute subs2) c1)) (cSuperset b (AnnVar pi))
                     )
w env (PCase term1 x1 x2 term2) = do
    (t1, subs1, c1) <- w env term1
    a1 <- fresh
    a2 <- fresh
    b <- freshAnnVar
    let env1 = (envAppend' [(x1, TypeScheme Set.empty a1), (x2, TypeScheme Set.empty a2)] (envSubstitute subs1 env))
    let subs2 = unify t1 (TypeGeneral b [a1, a2])
    (t2, subs3, c2) <- w (envSubstitute subs2 env1) term2
    return $ debug $ ( t2
                     , subs3 -.- subs2 -.- subs1
                     , cUnion c2 (conSubstitute (subs3 -.- subs2) c1)
                     )
w env (Cons pi term1 term2) = do
    (t1, subs1, c1) <- w env term1
    (t2, subs2, c2) <- w (envSubstitute subs1 env) term2
    b <- freshAnnVar
    let subs3 = unify (TypeGeneral b [subs2 -$- t1]) t2
    return $ debug $ ( subs3 -$- t2
                     , subs3 -.- subs2 -.- subs1
                     , cUnion (cUnion (conSubstitute subs3 c2) (conSubstitute (subs3 -.- subs2) c1)) (cSuperset b (AnnVar pi))
                     )
w env (Nil pi) = do
    a <- fresh
    b <- freshAnnVar
    return $ debug $ ( TypeGeneral b [a]
                     , idSub
                     , cSuperset b (AnnVar pi)
                     )
w env (LCase term1 x1 x2 term2 term3) = do
    (t1, subs1, c1) <- w env term1
    a <- fresh
    b <- freshAnnVar
    let env1 = (envAppend' [(x1, TypeScheme Set.empty a), (x2, TypeScheme Set.empty (TypeGeneral b [a]))] (envSubstitute subs1 env))
    let subs2 = unify t1 (TypeGeneral b [a])
    (t2, subs3, c2) <- w (envSubstitute subs2 env1) term2
    (t3, subs4, c3) <- w (envSubstitute (subs3 -.- subs2) env1) term3
    let subs5 = unify (subs4 -$- t2) t3
    return $ debug $ ( subs5 -$- t3
                     , subs5 -.- subs4 -.- subs3 -.- subs2 -.- subs1
                     , cUnion (conSubstitute subs5 c3) $ cUnion (conSubstitute (subs5 -.- subs4) c2) (conSubstitute (subs5 -.- subs4 -.- subs3 -.- subs2) c1)
                     )
w env (DType pi terms) = do
    (types, subs, cs) <- foldM (\(types, subs, cs) term -> do
        (t, s, c) <- w (envSubstitute subs env) term
        return (types ++ [t], s -.- subs, cUnion c (conSubstitute (s -.- subs) cs))) ([], idSub, cEmpty) terms
    b <- freshAnnVar
    return $ debug $ ( TypeGeneral b types
                     , subs
                     , cs
                     )
w env (DTCase term1 xs term2 term3) = do
    (t1, subs1, c1) <- w env term1
    as <- sequence $ take (length xs) $ repeat fresh
    b <- freshAnnVar
    let env1 = (envAppend' (zip xs (map (TypeScheme Set.empty) as)) (envSubstitute subs1 env))
    let subs2 = unify t1 (TypeGeneral b as)
    (t2, subs3, c2) <- w (envSubstitute subs2 env1) term2
    (t3, subs4, c3) <- w (envSubstitute (subs3 -.- subs2) env1) term3
    let subs5 = unify (subs4 -$- t2) t3
    return $ debug $ ( subs5 -$- t3
                     , subs5 -.- subs4 -.- subs3 -.- subs2 -.- subs1
                     , cUnion c2 (conSubstitute (subs3 -.- subs2) c1)
                     )
w env (Fn pi x t1) = do
    a1 <- fresh
    (t2, subs, c1) <- w (envAppend x (TypeScheme Set.empty a1) env) t1
    b <- freshAnnVar
    return $ debug $ ( TypeFn (subs -$- a1) b t2
                     , subs
                     , cUnion c1 (cSuperset b (AnnVar pi))
                     )
w env (Fun pi f x t1) = do
    a1 <- fresh
    a2 <- fresh
    b <- freshAnnVar
    (t2, subs1, c1) <- w (envAppend' [(f, TypeScheme Set.empty (TypeFn a1 b a2)), (x, TypeScheme Set.empty a1)] env) t1
    let subs2 = unify t2 (subs1 -$- a2)
    return $ debug $ ( TypeFn (subs2 -.- subs1 -$- a1) (subs2 -.- subs1 -$$- b) (subs2 -$- t2)
                     , subs2 -.- subs1
                     , cUnion (conSubstitute subs2 c1) (cSuperset (subs2 -.- subs1 -$$- b) (AnnVar pi))
                     )
w env (App term1 term2) = do
    (t1, subs1, c1) <- w env term1
    (t2, subs2, c2) <- w (envSubstitute subs1 env) term2
    a <- fresh
    pi <- freshAnnVar
    let subs3 = unify (subs2 -$- t1) (TypeFn t2 pi a)
    return $ debug $ ( subs3 -$- a
                     , subs3 -.- subs2 -.- subs1
                     , cUnion (conSubstitute subs3 c2) (conSubstitute (subs3 -.- subs2) c1)
                     )
w env (ITE term1 term2 term3) = do
    (t1, subs1, c1) <- w env term1
    let env1 = envSubstitute subs1 env
    (t2, subs2, c2) <- w env1 term2
    let env2 = envSubstitute subs2 env1
    (t3, subs3, c3) <- w env2 term3
    let subs4 = unify (subs3 -.- subs2 -$- t1) TypeBool
    let subs5 = unify (subs4 -.- subs3 -$- t2) (subs4 -$- t3)
    return $ debug $ ( subs5 -.- subs4 -$- t3
                     , subs5 -.- subs4 -.- subs3 -.- subs2 -.- subs1
                     , cUnion (conSubstitute (subs5 -.- subs4) c3)
                        (cUnion (conSubstitute (subs5 -.- subs4 -.- subs3) c2)
                        (conSubstitute (subs5 -.- subs4 -.- subs3 -.- subs2) c1))
                     )
w env (Let x term1 term2) = do
    (t1, subs1, c1) <- w env term1
    let env1 = envSubstitute subs1 env
    let env2 = debug $ envAppend x (generalize env1 t1) env1
    (t2, subs2, c2) <- w env2 term2
    return $ debug $ ( t2
                     , subs2 -.- subs1
                     , cUnion c2 (conSubstitute subs2 c1)
                     )
w env (Oper op term1 term2) = do
    (t1, subs1, c1) <- w env term1
    let env1 = envSubstitute subs1 env
    (t2, subs2, c2) <- w env1 term2
    let (param1, param2, ret) = opTypes op
    let subs3 = unify (subs2 -$- t1) param1
    let subs4 = unify (subs3 -$- t2) param2
    return $ debug $ ( ret
                     , subs4 -.- subs3 -.- subs2 -.- subs1
                     , cUnion (conSubstitute (subs4 -.- subs3) c2) (conSubstitute (subs4 -.- subs3 -.- subs2) c1)
                     )

opTypes :: Op -> (Type, Type, Type)
opTypes op | op `elem` [Add, Sub, Mul, Div] = (TypeInt, TypeInt, TypeInt)
           | otherwise = error "This is impossible"

debug :: Show a => a -> a
--debug = Debug.traceShowId
debug = id
