import qualified Data.Set as Set

import FunFlow.Lib
import FunFlow.TypeSystem

examples :: [(String, Type)]
examples = [ ("add", TypeInt)
           , ("apply_id", TypeInt)
           , ("id", TypeFn (Alpha 0) Set.empty (Alpha 0))
           , ("fun_id", TypeFn (Alpha 0) Set.empty (Alpha 0))
           , ("fun_if", TypeFn (Alpha 0) Set.empty (Alpha 0))
           , ("if_let", TypeInt)
           , ("let_bool", TypeBool)
           , ("let_id", TypeFn (Alpha 0) Set.empty (Alpha 0))
           , ("let_if", TypeInt)
           , ("let_int", TypeInt)
           , ("let_x", TypeInt)
           ]

main :: IO ()
main = do
    results <- sequence $ map checkExample examples
    let succeed = length $ filter id results
    let total = length examples
    putStrLn $ "Passed " ++ show succeed ++ " of " ++ show total ++ " tests"

checkExample :: (String, Type) -> IO Bool
checkExample (name, expectedType) = do
    let path = "examples/" ++ name ++ ".fun"
    t <- typeCheck path
    case tryUnify t expectedType of
        Right _ -> do
            putStrLn $ "Passed (" ++ name ++ ") with type " ++ show t
            return True
        Left err -> do
            putStrLn $ "Failed (" ++ name ++ "):" ++ err
            return False
