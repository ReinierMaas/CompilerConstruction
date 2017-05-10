import Test.QuickCheck
import Programs

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
wrongProgramsDontParse = all not <$> sequence (map parsesCorrectly incorrectPrograms)

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
    isJust <$> (hSilence [stdout, stderr] $ ioRun' typeCheckATerm aterm)
