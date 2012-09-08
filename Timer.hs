module Timer (TimerReel, Timer
             , startTimerReel, stopTimerReel
             , addTimer, cancelTimer
             ) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.STM
import Data.List (null, insertBy)
import Data.Time.Clock (UTCTime, getCurrentTime, diffUTCTime, addUTCTime)
import System.Timeout (timeout)

data Command = Refresh | Exit

data TimerReel = TimerReel { rCmd :: TMVar Command
                           , rList :: TVar [Timer]
                           , rSeq  :: TVar Integer
                           }

-- A very primitive and inefficient implementation.
-- Add is O(N), remove is O(N), dispatch is O(1).
-- TODO: replace with Data.Map?
data Timer = Timer { tId     :: Integer
                   , tTime   :: UTCTime
                   , tAction :: IO ()
                   }

startTimerReel :: IO TimerReel
startTimerReel = do
    w <- newEmptyTMVarIO
    l <- newTVarIO []
    s <- newTVarIO 0
    let tr = TimerReel w l s
    forkIO $ runTimerReel tr
    return tr

stopTimerReel :: TimerReel -> IO ()
stopTimerReel reel = atomically $ putTMVar (rCmd reel) Exit

runTimerReel :: TimerReel -> IO ()
runTimerReel r = do
    usecs <- usecsToSleep
    res <- timeout usecs (atomically $ takeTMVar (rCmd r))
    case res of
        Just Refresh -> dispatch >> runTimerReel r
        Nothing      -> dispatch >> runTimerReel r
        Just Exit    -> return ()
  where
    usecsToSleep = do
        now <- getCurrentTime
        atomically $ do
            timers <- readTVar $ rList r
            return $ case timers of
                [] -> -1
                tm:tms -> fromIntegral $ getDelta now (tTime tm)

    getDelta now t =
        let secs = realToFrac $ diffUTCTime t now
            usecs = (ceiling $ secs * 10^6) :: Integer
        in min (max 0 usecs) (fromIntegral (maxBound :: Int))
        
    dispatch = do
        now <- getCurrentTime
        tm <- atomically $ do
            timers <- readTVar $ rList r
            case timers of
                []   -> return Nothing
                t:ts -> do
                    if tTime t < now
                        then writeTVar (rList r) ts >> return (Just t)
                        else return Nothing
        case tm of
            Nothing -> return ()
            Just t  -> tAction t >> dispatch


addTimer :: TimerReel -> Double -> IO () -> IO Timer
addTimer reel dt action = do
    now <- getCurrentTime
    let expire = addUTCTime (realToFrac dt) now
    atomically $ do
        sq <- readTVar $ rSeq reel
        let tm = Timer sq expire action
        modifyTVar (rSeq reel) (+1)
        modifyTVar (rList reel) $ insertBy compareTimers tm 
        putTMVar (rCmd reel) Refresh
        return tm
  where
    compareTimers a b = compare (tTime a) (tTime b)


cancelTimer :: TimerReel -> Timer -> IO ()
cancelTimer reel tm = atomically $ do 
    modifyTVar (rList reel) rm
  where
    rm = filter (\x -> tId x /= tId tm)


