module Main where

import qualified BatchTests (runTests)
import qualified LawsTests (runTests)

import System.Exit

runTests = fmap or $ sequence
    [ LawsTests.runTests
    , BatchTests.runTests
    ]

main = do
    result <- runTests
    if result
        then return ()
        else do
            putStrLn "Tests FAILED"
            exitWith $ ExitFailure 1
