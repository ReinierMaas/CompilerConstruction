import qualified Data.Set as Set

import FunFlow.Lib
import FunFlow.TypeSystem

examples :: [(String, Type)]
examples = [ ("add", TypeInt)
           , ("apply_id", TypeInt)
           , ("id", TypeFn (Alpha 0) (AnnVar 1) (Alpha 0))
           , ("if_fn", TypeFn (Alpha 0) (AnnVar 1) (Alpha 0))
           , ("fun_id", TypeFn (Alpha 0) (AnnVar 1) (Alpha 0))
           , ("fun_if", TypeFn (Alpha 0) (AnnVar 1) (Alpha 0))
           , ("if_let", TypeInt)
           , ("list", TypeGeneral (AnnVar 1) [TypeInt])
           , ("lcase", TypeInt)
           , ("lcase_fn", TypeFn (Alpha 0) (AnnVar 1) (Alpha 0))
           , ("let_bool", TypeBool)
           , ("let_id", TypeFn (Alpha 0) (AnnVar 1) (Alpha 0))
           , ("let_if", TypeInt)
           , ("let_int", TypeInt)
           , ("let_x", TypeInt)
           , ("pair", TypeGeneral (AnnVar 1) [TypeInt, TypeBool])
           , ("pcase", TypeInt)
           , ("pcase_fn", TypeFn TypeInt (AnnVar 1) TypeInt)
           ]

main :: IO ()
main = do
    results <- sequence $ map checkExample examples
    let succeed = length $ filter id results
    let total = length examples
    putStrLn $ "Passed " ++ show succeed ++ " of " ++ show total ++ " tests"

checkExample :: (String, Type) -> IO Bool
checkExample (name, expectedType) = do
    p <- parse $ "examples/" ++ name ++ ".fun"
    let (t, c, p') = typeCheck p
    case tryUnify t expectedType of
        Right _ -> do
            putStrLn $ "Passed (" ++ name ++ ") with type " ++ show t
            putStrLn $ "=> ast: " ++ show p'
            putStrLn $ "=> constraints: " ++ show c
            return True
        Left err -> do
            putStrLn $ "Failed (" ++ name ++ "):" ++ err
            return False
