import Test.QuickCheck
import CCO.Component (ioRun')
import CCO.Feedback (runFeedback)
import CCO.Tree (ATerm, Tree (fromTree))
import CCO.TypeCheck (typeCheckATerm)
import Data.Maybe (isJust)
import System.IO (stderr, stdout)
import System.IO.Error (tryIOError)
import System.IO.Silently (hCapture, hSilence)

import qualified CCO.Diag as D

main :: IO ()
main = do
    correctProgramsParse >>= quickCheck
    typeIncorrectProgramsParse >>= quickCheck
    wrongProgramsDontParse >>= quickCheck
    correctProgramsTypeCheck >>= quickCheck
    typeIncorrectProgramsDontTypeCheck >>= quickCheck

correctProgramsParse :: IO Bool
correctProgramsParse = all id <$> sequence (map parsesCorrectly correctPrograms)

typeIncorrectProgramsParse :: IO Bool
typeIncorrectProgramsParse = all id <$> sequence (map parsesCorrectly typeIncorrectPrograms)

wrongProgramsDontParse :: IO Bool
wrongProgramsDontParse = all not <$> sequence (map parsesCorrectly wrongPrograms)

correctProgramsTypeCheck :: IO Bool
correctProgramsTypeCheck = all id <$> sequence (map typeChecks correctPrograms)

typeIncorrectProgramsDontTypeCheck :: IO Bool
typeIncorrectProgramsDontTypeCheck = all not <$> sequence (map typeChecks typeIncorrectPrograms)

-- Misc stuff

parseDiagAsATerm :: String -> IO (Maybe ATerm)
parseDiagAsATerm p = do
    result <- hSilence [stdout, stderr] $ ioRun' D.parser p
    return (fromTree <$> result)

parsesCorrectly :: String -> IO Bool
parsesCorrectly s = isJust <$> parseDiagAsATerm s

typeChecks :: String -> IO Bool
typeChecks p = do
    Just aterm <- parseDiagAsATerm p
    result <- hSilence [stdout] $ ioRun' typeCheckATerm aterm
    return $ isJust result

-- Test programs

correctPrograms :: [String]
correctPrograms = [
    "program hello in Haskell",
    "platform x86",
    "execute program hello in Haskell on interpreter hugs for Haskell in x86-windows end",
    "execute interpreter hugs for Haskell in x86-windows on platform x86-windows end",
    "execute program hello in Haskell on interpreter hugs for Haskell in i686-windows end",
    "compile program hello in UUAG with compiler uuagc from UUAG to Haskell in i686-windows end",
    "execute compile program hello in UUAG with compiler uuagc from UUAG to Haskell in i686-windows end on interpreter hugs for Haskell in i686-windows end",
    "execute execute compile program hello in UUAG with compiler uuagc from UUAG to Haskell in i686-windows end on platform i686-windows end on interpreter hugs for Haskell in i686-windows end" ]

typeIncorrectPrograms :: [String]
typeIncorrectPrograms = [
    "execute platform x86 on platform ARM end", -- 1
    "execute platform x86 on platform x86 end", -- 1
    "execute program hello in Haskell on program world in Haskell end", -- 2
    "execute interpreter hugs for Haskell in x86-windows on compiler hugs from Haskell to x86-windows in x86-windows end", -- 2
    "execute compile program hello in UUAG with compiler uuagc from UUAG to Haskell in i686-windows end on interpreter x64-on-arm for x64 in arm end", -- 3
    "compile platform x64 with compiler x64-to-x86 from x64 to x86 in x64 end", -- 4
    "compile program hello in x64 with platform x64 end", -- 5
    "compile program hello in Haskell with compiler uuagc from UUAG to Haskell in i686-windows end" -- 6
    ]

wrongPrograms :: [String]
wrongPrograms = [
    "program hello",
    "platform" ]
