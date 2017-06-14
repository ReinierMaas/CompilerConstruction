import Test.QuickCheck
import FunFlow.Lib

examples :: [(String, Type)]
examples = [ ("add", TypeInteger)
           , ("apply_id", TypeInteger)
           , ("id", TypeFn (Alpha 0) (Alpha 0))
           , ("if_let", TypeInteger)
           , ("let_bool", TypeBool)
           , ("let_id", TypeFn (Alpha 0) (Alpha 0))
           , ("let_if", TypeInteger)
           , ("let_int", TypeInteger)
           , ("let_x", TypeInteger)
           ]

main :: IO ()
main = do
    map checkExample examples

checkExample :: (String, Type) -> IO Bool
checkExample (path, expectedType) = do
    t <- typeCheck path
