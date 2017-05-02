import Test.QuickCheck
import CCO.Component (ioRun')
import CCO.Feedback (runFeedback)
import System.IO (stderr)
import System.IO.Error (tryIOError)
import System.IO.Silently (hSilence)
import qualified CCO.Diag as D

main :: IO ()
main = do
    testCorrectPrograms >>= quickCheck
    testWrongPrograms >>= quickCheck

testCorrectPrograms :: IO Bool
testCorrectPrograms = all id <$> sequence (map correct correctPrograms)

testWrongPrograms :: IO Bool
testWrongPrograms = all not <$> sequence (map correct wrongPrograms)

correct :: String -> IO Bool
correct p = do
    result <- hSilence [stderr] $ ioRun' D.parser p
    case result of
        Just _  -> return True
        Nothing -> return False

correctPrograms :: [String]
correctPrograms = [
    "program hello in Haskell",
    "platform x86" ]

wrongPrograms :: [String]
wrongPrograms = [
    "program hello",
    "platform" ]
