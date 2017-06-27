import Examples (testType, useId, wrongId)

main = do
    putStrLn "--- useId ---"
    putStrLn $ show $ testType useId
    putStrLn "--- wrongId ---"
    putStrLn $ show $ testType wrongId