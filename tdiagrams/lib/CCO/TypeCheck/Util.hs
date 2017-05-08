module CCO.TypeCheck.Util where

data Type = Program String
          | Platform String
          | Interpreter String String
          | Compiler String String String
          | Compiling String String
          | Execute
          | Error String
          deriving (Eq, Show)

asProgram :: Type -> Maybe String
asProgram (Program l)       = Just l
asProgram (Interpreter _ m) = Just m
asProgram (Compiler _ _ m)  = Just m
asProgram _                 = Nothing

asPlatform :: Type -> Maybe String
asPlatform (Platform m)      = Just m
asPlatform (Interpreter l _) = Just l
asPlatform _                 = Nothing

compare :: (Type -> Maybe String) -> Type -> Type -> Bool
compare = undefined

executeCombine :: Type -> Type -> Type
executeCombine (Compiling o m1) plat =
    case asPlatform plat of
        Just m2 -> if m2 == m1
                   then Program o
                   else if m2 == o
                       then Execute
                       else Error $ show m1 ++ " should be equal to either " ++ m2 ++ " or " ++ o
        Nothing -> Error $ show plat ++ " is not a subtype of Platform " ++ m1
executeCombine prog plat =
    case asProgram prog of
        Just m1 -> case asPlatform plat of
            Just m2 -> if m1 == m2
                       then Execute
                       else Error $ m1 ++ " != " ++ m2
            Nothing -> Error $ show plat ++ " is not a subtype of Platform " ++ m1
        Nothing -> Error $ show prog ++ " is not a subtype of Program"

compileCombine :: Type -> Type -> Type
compileCombine prog (Compiler l1 o m) =
    case asProgram prog of
        Just l2 -> if l1 == l2
                   then Compiling o m
                   else err
        Nothing -> err
    where err = Error $ show prog ++ " is not a subtype of Program " ++ l1
compileCombine t1 t2 = Error $ "Compile between incompatible types: " ++ show t1 ++ ", " ++ show t2
