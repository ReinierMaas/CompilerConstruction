module TypeSystem where

import Ast
import Control.Monad.State.Lazy (State, get, put)
import Data.List (find)
import Data.Maybe (fromJust)

{- Environment -}
type TypeEnv = [(Name, TypeScheme)]

envLookup :: Name -> TypeEnv -> Maybe TypeScheme
envLookup x env = snd <$> find (\(name, scheme) -> name == x) env

envAppend :: Name -> TypeScheme -> TypeEnv -> TypeEnv
envAppend x t env = (x, t) : env

envAppend' :: [(Name, TypeScheme)] -> TypeEnv -> TypeEnv
envAppend' new = ((reverse new) ++)

envSubstitute :: TypeSubstitution -> TypeEnv -> TypeEnv
envSubstitute subs env = undefined

{- Substitutions -}
type TypeSubstitution = Type -> Type

substitute :: Type -> Type -> TypeSubstitution
substitute t1 t2 k = if k == t1 then t2 else k

{- Other stuff -}
data Type = TypeInteger
          | TypeBool
          | TypeFn Type Type
          | Alpha Int
          deriving (Eq, Show)

data TypeScheme = SchemeType Type
                | ForAll Int TypeScheme

generalize :: TypeEnv -> Type -> TypeScheme
generalize = undefined

instantiate :: TypeScheme -> Type
instantiate = undefined

{- Unification -}
unify :: Type -> Type -> TypeSubstitution
unify TypeInteger TypeInteger = id
unify TypeBool TypeBool = id
unify (TypeFn t1 t2) (TypeFn t3 t4) = subs2 . subs1
    where
    subs1 = unify t1 t3
    subs2 = unify (subs1 t2) (subs1 t4)
unify a@(Alpha x) t = if not (x `isFreeIn` t) then substitute a t else unifyFailure a t
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
w _ (Integer _) = return (TypeInteger, id)
w _ (Bool _) = return (TypeBool, id)
w env (Var x) = undefined -- See slides on page 24. Unclear how to implement.
w env (Fn pi x t1) = do
    a1 <- fresh
    (t2, subs) <- w (envAppend x (SchemeType a1) env) t1
    return $ (TypeFn (subs a1) t2, subs)
w env (Fun pi f x t1) = do
    a1 <- fresh
    a2 <- fresh
    (t2, subs1) <- w (envAppend' [(f, SchemeType (TypeFn a1 a2)), (x, SchemeType a1)] env) t1
    let subs2 = unify t2 (subs1 a2)
    return (TypeFn (subs2 (subs1 a1)) (subs2 t2), subs2 . subs1)
w env (App term1 term2) = do
    (t1, subs1) <- w env term1
    (t2, subs2) <- w (envSubstitute subs1 env) term2
    a <- fresh
    let subs3 = unify (subs2 t1) (TypeFn t2 a)
    return (subs3 a, subs3 . subs2 . subs1)
w env (ITE term1 term2 term3) = do
    (t1, subs1) <- w env term1
    let env = envSubstitute subs1 env
    (t2, subs2) <- w env term2
    let env = envSubstitute subs2 env
    (t3, subs3) <- w env term3
    let subs4 = unify (subs3 (subs2 t1)) TypeBool
    let subs5 = unify (subs4 (subs3 t2)) (subs4 t3)
    return (subs5 (subs4 t3), subs5 . subs4 . subs3 . subs2 . subs1)
w env (Let x term1 term2) = do
    (t1, subs1) <- w env term1
    let env = envSubstitute subs1 env
    let env = envAppend x (generalize env t1) env
    (t2, subs2) <- w env term2
    return (t2, subs2 . subs1)
w env (Oper op term1 term2) = do
    (t1, subs1) <- w env term1
    let env = envSubstitute subs1 env
    (t2, subs2) <- w env term2
    let (param1, param2, ret) = opTypes op
    let subs3 = unify (subs2 t1) param1
    let subs4 = unify (subs3 t2) param2
    return (ret, subs4 . subs3 . subs2 . subs1)

opTypes :: Op -> (Type, Type, Type)
opTypes op | op `elem` [Add, Sub, Mul, Div] = (TypeInteger, TypeInteger, TypeInteger)
           | otherwise = error "This is impossible"
