import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar (MVar, newMVar, takeMVar, putMVar)
import Control.Concurrent.Chan (Chan, newChan, readChan, writeChan)
import Control.Monad (liftM, forever)
import Data.List (null, insertBy)
import Data.Time.Clock (UTCTime, getCurrentTime, diffUTCTime, addUTCTime)
import System.Timeout (timeout)

data TimerReel = TimerReel {
                     reelChan :: Chan TMessage,
                     reelSeq :: MVar Integer
                 }

data Timer = Timer {
                 timerId :: Integer,
                 timerT  :: UTCTime,
                 timerF  :: IO ()
             }

data TMessage = AddTimer Timer
              | CancelTimer Integer


newTimerReel :: IO TimerReel
newTimerReel = do
    chan <- newChan
    seq <- newMVar 0
    forkIO $ runTimerReel chan []
    return $ TimerReel chan seq


-- A very primitive and inefficient implementation.
-- Add is O(N), remove is O(N), dispatch is O(1).
runTimerReel :: Chan TMessage -> [Timer] -> IO ()
runTimerReel chan timers = do
    now <- getCurrentTime
    let dt = if null timers
                then -1 -- indefinetely
                else let secs = realToFrac $ diffUTCTime (timerT $ head timers) now
                         usecs = (floor $ secs * 10^6) :: Integer
                     in  min (max 0 usecs) (fromIntegral (maxBound :: Int))
    result <- timeout (fromIntegral dt) (readChan chan)
    case result of
        Nothing ->
            dispatch timers >>= runTimerReel chan
        Just (AddTimer new) ->
            runTimerReel chan $ insertBy compareTimers new timers
        Just (CancelTimer tid) ->
            runTimerReel chan $ filter (\tm -> timerId tm /= tid) timers
  where
    compareTimers a b = compare (timerT a) (timerT b)
    dispatch timers = do
        if null timers
             then return timers
             else do
                 let tm = head timers
                 now <- getCurrentTime
                 if timerT tm < now
                     then timerF tm >> dispatch (tail timers)
                     else return timers


-- returns IO() that cancells created timer
addTimer :: TimerReel -> Double -> IO () -> IO (IO ())
addTimer reel dt action = do
    sq <- takeMVar (reelSeq reel)
    putMVar (reelSeq reel) $! (sq + 1)
    expire <- liftM (addUTCTime $ realToFrac dt) getCurrentTime
    let tm = Timer sq expire action
    writeChan (reelChan reel) (AddTimer tm)
    return $ cancelTimer reel sq


cancelTimer :: TimerReel -> Integer -> IO ()
cancelTimer reel tid = writeChan (reelChan reel) (CancelTimer tid)

main :: IO ()
main = do
    tr <- newTimerReel
    threadDelay 1000000
    c1 <- addTimer tr 0.5 $ print "asd"
    threadDelay 1000000
    c2 <- addTimer tr 3.5 $ print "qwe"
    threadDelay 1000000
    c2
    c3 <- addTimer tr 1.5 $ print "zxc"
    forever $ threadDelay 1000
