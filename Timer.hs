module Timer (TimerReel, Timer, newTimerReel, addTimer) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.STM
--import Control.Monad (liftM, forever)
import Data.List (null, insertBy)
import Data.Time.Clock (UTCTime, getCurrentTime, diffUTCTime, addUTCTime)
import System.Timeout (timeout)

data Command = Refresh | Exit

data TimerReel = TimerReel { rCmd :: TMVar Command
                           , rList :: TVar [Timer]
                           , rSeq  :: TVar Integer
                           }

data Timer = Timer { tId     :: Integer
                   , tTime   :: UTCTime
                   , tAction :: IO ()
                   }

newTimerReel :: IO TimerReel
newTimerReel = do
    w <- newEmptyTMVarIO
    l <- newTVarIO []
    s <- newTVarIO 0
    let tr = TimerReel w l s
    forkIO $ runTimerReel tr
    return tr


-- A very primitive and inefficient implementation.
-- Add is O(N), remove is O(N), dispatch is O(1).
runTimerReel :: TimerReel -> IO ()
runTimerReel r = do
    now <- getCurrentTime
    usecs <- atomically $ do
        timers <- readTVar $ rList r
        return $ case timers of
            [] -> -1
            tm:tms -> getDelta now (tTime tm)

    res <- timeout (fromIntegral usecs) (atomically $ takeTMVar (rCmd r))
    case res of
        Just Refresh -> dispatch >> runTimerReel r
        Nothing      -> dispatch >> runTimerReel r
        Just Exit    -> return ()
  where
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


-- returns Timer structure
addTimer :: TimerReel -> Double -> IO () -> IO ()
addTimer reel dt action = do
    now <- getCurrentTime
    let expire = addUTCTime (realToFrac dt) now
    atomically $ do
        sq <- readTVar $ rSeq reel
        modifyTVar (rSeq reel) (+ 1)
        let tm = Timer sq expire action
        modifyTVar (rList reel) $ insertBy compareTimers tm 
        putTMVar (rCmd reel) Refresh
  where
    compareTimers a b = compare (tTime a) (tTime b)


cancelTimer :: TimerReel -> Timer -> IO ()
cancelTimer reel tm = atomically $ do 
    modifyTVar (rList reel) rm
  where
    rm = filter (\x -> tId x /= tId tm)

modifyTVar :: TVar a -> (a -> a) -> STM ()
modifyTVar x f = readTVar x >>= writeTVar x . f

