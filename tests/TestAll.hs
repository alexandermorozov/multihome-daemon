module Main where
import System.Exit (exitFailure)
import Test.HUnit

main = do
    putStrLn "This test always fails!"
    exitFailure


test1 = TestCase (assertEqual "1 == 1" 1 1)

tests = TestList [TestLabel "test1" test1]

