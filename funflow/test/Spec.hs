import FunFlow.Lib
import FunFlow.TypeSystem

examples :: [(String, Type)]
examples = [ ("add", TypeInteger)
           , ("apply_id", TypeInteger)
           , ("id", TypeFn (Alpha 0) (Alpha 0))
           , ("fun_id", TypeFn (Alpha 0) (Alpha 0))
           , ("fun_if", TypeFn (Alpha 0) (Alpha 0))
           , ("if_let", TypeInteger)
           , ("let_bool", TypeBool)
           , ("let_id", TypeFn (Alpha 0) (Alpha 0))
           , ("let_if", TypeInteger)
           , ("let_int", TypeInteger)
           , ("let_x", TypeInteger)
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
        Right _ -> return True
        Left err -> putStrLn err >> return False
