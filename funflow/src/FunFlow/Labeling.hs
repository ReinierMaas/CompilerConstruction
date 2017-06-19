module FunFlow.Labeling (
    label
) where

import Control.Monad.State.Lazy (State, evalState, get, put)
import FunFlow.Ast

fresh :: State Int Int
fresh = do
    x <- get
    put $ x + 1
    return x

label :: Expr -> State Int Expr
label i@(Int _) = return i
label b@(Bool _) = return b
label v@(Var _) = return v
label (Pair _ e1 e2) = Pair <$> fresh <*> pure e1 <*> pure e2
label (PCase e1 n1 n2 e2) = PCase <$> label e1 <*> pure n1 <*> pure n2 <*> label e2
label (Fun _ n1 n2 expr) = Fun <$> fresh <*> pure n1 <*> pure n2 <*> label expr
label (Fn _ n expr) = Fn <$> fresh <*> pure n <*> label expr
label (App expr1 expr2) = App <$> label expr1 <*> label expr2
label (Let n expr1 expr2) = Let <$> pure n <*> label expr1 <*> label expr2
label (ITE expr1 expr2 expr3) = ITE <$> label expr1 <*> label expr2 <*> label expr3
label (Oper op expr1 expr2) = Oper <$> pure op <*> label expr1 <*> label expr2
