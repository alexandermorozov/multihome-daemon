import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.Chan (Chan, newChan, readChan, writeChan)
import Control.Monad (liftM, forever)
import Data.List (null, sortBy)
import Data.Time.Clock (UTCTime, getCurrentTime, diffUTCTime, addUTCTime)
import System.Timeout (timeout)

data TimerReel = TimerReel {reelChan :: Chan TMessage}

data Timer = Timer {timerId :: Integer, timerT :: UTCTime, timerF :: IO ()}

data TMessage = AddTimer Double (IO ())
              | CancelTimer


newTimerReel :: IO TimerReel
newTimerReel = do
    chan <- newChan
    forkIO $ runTimerReel chan 0 []
    return $ TimerReel chan


-- a very primitive and inefficient implementation
runTimerReel :: Chan TMessage -> Integer -> [Timer] -> IO ()
runTimerReel chan sequence timers = do
    now <- getCurrentTime
    let dt = if null timers
                then -1 -- indefinetely
                else let secs = realToFrac $ diffUTCTime (timerT $ head timers) now
                         usecs = (floor $ secs * 10^6) :: Integer
                     in  min (max 0 usecs) (fromIntegral (maxBound :: Int))
    result <- timeout (fromIntegral dt) (readChan chan)
    case result of
        Nothing -> dispatch timers >>= runTimerReel chan sequence
        Just (AddTimer dt f) -> do
            expire <- liftM (addUTCTime $ realToFrac dt) getCurrentTime
            let new = Timer sequence expire f
                timers' = sortBy compareTimers (new:timers)
            runTimerReel chan (sequence+1) timers'
        -- fixme
  where
    compareTimers a b = compare (timerT a) (timerT b)
    dispatch :: [Timer] -> IO [Timer]
    dispatch timers = do
        if null timers
             then return timers
             else do
                 let tm = head timers
                 now <- getCurrentTime
                 if timerT tm < now
                     then timerF tm >> return (tail timers)
                     else return timers

addTimer :: TimerReel -> Double -> IO () -> IO ()
addTimer reel dt action =
    writeChan (reelChan reel) (AddTimer dt action)

main :: IO ()
main = do
    tr <- newTimerReel
    threadDelay 1000000
    addTimer tr 0.5 $ print "asd"
    addTimer tr 2.5 $ print "qwe"
    addTimer tr 1.5 $ print "zxc"
    forever $ threadDelay 1000
