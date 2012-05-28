module Main where

import Control.Concurrent (threadDelay)
import Control.Concurrent.MVar (newMVar, takeMVar, modifyMVar_)
import Control.Monad (unless)
import qualified Data.Set as S
import Data.List (foldl')
import Data.Time.Clock (getCurrentTime, diffUTCTime)
import System.Exit (exitFailure, exitSuccess)
import Test.HUnit

import Timer

-- timer firing delays bigger than this value are unacceptable
maxDelay = 0.03


main = do
    count <- runTestTT tests
    if (errors count > 0 || failures count >0)
       then exitFailure
       else exitSuccess

tests = TestList [TestLabel "timer test 1" test1]

test1 = TestCase $ do
    events <- newMVar []
    reel <- newTimerReel
    let add = addTm reel events
    tm1 <- add 1 0.3
    tm2 <- add 2 0.6
    tm3 <- add 3 0.7
    tm4 <- add 4 0.2
    tm5 <- add 5 0.2
    cancelTimer reel tm2

    threadDelay $ 1000000
    es <- takeMVar events
    unless (allOk es [1,3,4,5]) (assertFailure $ show es)
    return ()
  where
    addTm reel events num dt = do
        t0 <- getCurrentTime
        addTimer reel dt $ do
            t1 <- getCurrentTime
            let diff = realToFrac (diffUTCTime t1 t0) - dt
            modifyMVar_ events $ return . ((num, diff) :)
    allOk :: [(Int, Double)] -> [Int] -> Bool
    allOk events expected =
        let fired = S.fromList $ map fst events
            exp   = S.fromList expected
            intime = map (\(_, x) -> x > 0 && x < maxDelay) events
        in fired == exp && foldl' (&&) True intime

