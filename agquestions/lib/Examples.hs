module Examples where

import AgQuestions.AG (Expression(..), Ty(..), sem_Expression)

testType :: Expression -> Maybe Ty
testType expr = (sem_Expression expr) []

useId :: Expression
useId = Let "id" (Fun Nat Nat) (Lambda "x" Nat (Variable "x")) (Application (Variable "id") (Constant 5))

wrongId :: Expression
wrongId = Let "id" Nat (Lambda "x" Nat (Variable "x")) (Application (Variable "id") (Constant 5))
