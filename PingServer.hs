--module PingServer where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.Chan (Chan, newChan, readChan, writeChan)
import Control.Monad (liftM, forever, when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State (StateT, execStateT, modify, gets, get, put)
import qualified Data.ByteString as SB
import qualified Data.ByteString.Lazy as LB
import Data.Foldable (toList)
import Data.Maybe (isNothing, fromJust)
import qualified Data.Map as Map
import Data.Time.Clock (UTCTime)
import Data.Word (Word32)
import qualified Data.Dequeue as DQ
import Network.BSD (getProtocolByName, protoNumber)
import Network.Socket (withSocketsDo, socket, inet_addr, Socket,
                       HostAddress, Family (AF_INET), SocketType (Raw),
                       SockAddr (SockAddrInet))
import Network.Socket.ByteString (sendTo, recvFrom)

import Icmp (IpPacket(..), IcmpEchoPacket(..),
             dumpIcmpEchoRequest, parseIp, parseIcmpEchoReply)

import Timer
import SockOpt

type IP = Word32

data PingServer = PingServer {serverChan :: Chan Command}

data PacketId = PacketId { pktIp :: IP
                         , pktId :: Int
                         , pktSeq :: Int
                         } deriving (Show, Eq)

data ServerState = ServerState { srvIps :: DQ.BankersDequeue IP
                               , srvTReel :: TimerReel
                               , srvSentPackets :: Map.Map PacketId UTCTime
                               , srvPingInterval :: Maybe Double
                               , srvTimeoutInterval :: Maybe Double
                               , srvTimeoutAction :: Maybe (IO ())
                               , srvLinkTimeoutCancel :: Maybe (IO ())
                               }



data Command = AddHost String
             | DelHost String
             | SetPingInterval Double
             | SetTimeoutInterval Double
             | SetTimeoutAction (IO ())
             | Exit

             | StartPing
             | PingReceived IpPacket IcmpEchoPacket
             | PingTimedOut PacketId
             | LinkTimeout


instance Ord PacketId where
    (PacketId a1 b1 c1) <= (PacketId a2 b2 c2) =
        (a1 <= a2) || (b1 <= b2) || (c1 <= c2)

main = withSocketsDo $ do
    addr <- inet_addr "192.168.3.2"
    ps <- newPingServer "br1"
    addHost ps "192.168.3.2"
    setTimeoutInterval ps 7
    setPingInterval ps 3
    forever $ threadDelay 1


newPingServer :: String -> IO PingServer
newPingServer interface = do
    chan <- newChan
    s <- openSocket interface
    forkIO $ runServer s chan
    return $ PingServer chan
  where
    openSocket interface = do
        proto <- getProtocolByName "icmp"
        s <- socket AF_INET Raw (protoNumber proto)
        setSocketBindToDevice s interface
        return s


addHost :: PingServer -> String -> IO ()
addHost ps = sendC ps . AddHost

delHost :: PingServer -> String -> IO ()
delHost ps = sendC ps . DelHost

setTimeoutInterval :: PingServer -> Double -> IO ()
setTimeoutInterval ps = sendC ps . SetTimeoutInterval

setTimeoutAction :: PingServer -> IO () -> IO ()
setTimeoutAction ps = sendC ps . SetTimeoutAction

setTimeout :: PingServer -> Double -> IO ()
setTimeout ps = sendC ps . SetTimeoutInterval

setPingInterval :: PingServer -> Double -> IO ()
setPingInterval ps = sendC ps . SetPingInterval

sendC :: PingServer -> Command -> IO ()
sendC ps = writeChan (serverChan ps)


runServer :: Socket -> Chan Command -> IO ()
runServer sock chan = do
    forkIO $ socketListener sock chan
    tReel <- startTimerReel
    let state = ServerState { srvIps = DQ.empty
                            , srvTReel = tReel
                            , srvSentPackets = Map.empty
                            , srvPingInterval = Nothing
                            , srvTimeoutInterval = Nothing
                            , srvTimeoutAction = Nothing
                            , srvLinkTimeoutCancel = Nothing
                            }
    execStateT loop state
    return ()
  where
    loop :: StateT ServerState IO ()
    loop = do
        cmd <- liftIO $ readChan chan
        s <- get
        case cmd of
            AddHost h -> do
                addr <- liftIO $ inet_addr h
                put $ s {srvIps = DQ.pushFront (srvIps s) addr}
                loop
            DelHost h -> do
                addr <- liftIO $ inet_addr h
                put $ s {srvIps = filterAddr addr $ srvIps s}
                loop
            SetPingInterval dt -> do
                when (isNothing $ srvPingInterval s) $
                        liftIO $ writeChan chan StartPing
                put $ s {srvPingInterval = Just dt}
                loop
            SetTimeoutInterval dt -> do
                put $ s {srvTimeoutInterval = Just dt}
                when (isNothing $ srvLinkTimeoutCancel s) $ do
                    tm <- addTm dt (send LinkTimeout)
                    let cancel = cancelTimer (srvTReel s) tm
                    modify (\s1 -> s1 {srvLinkTimeoutCancel = Just cancel})
                loop
            StartPing -> do
                liftIO $ print "Start ping"
                addTm (fromJust $ srvPingInterval s) (send StartPing)
                let echo = dumpIcmpEchoRequest $ IcmpEchoPacket 10 10 SB.empty
                let ip = (fromJust $ DQ.first $ srvIps s)
                liftIO $ sendTo sock (toStrictBS echo) (SockAddrInet 0 ip)
                loop
            PingReceived ip echo -> do
                liftIO $ print $ "Got ping from " ++ show(ip) ++ " " ++ show(echo)
                loop
            LinkTimeout -> do
                liftIO $ print "Link timeout"
                loop
            Exit -> return ()
            --StartPing -> startPing
    filterAddr addr addrs =  DQ.fromList $ filter (/= addr) (toList addrs)
    send = writeChan chan
    addTm dt act = do
        r <- gets srvTReel
        liftIO $ addTimer r dt act
    toStrictBS = SB.concat . LB.toChunks


socketListener :: Socket -> Chan Command -> IO ()
socketListener s chan = do
    forever $ do
        (p, saddr) <- recvFrom s 65536
        case parseEcho p of
             Left errMsg -> print errMsg
             Right (ip, echo) -> writeChan chan $ PingReceived ip echo
  where
    parseEcho p = do
        ip <- parseIp p
        echo <- parseIcmpEchoReply $ ipPayload ip
        return (ip, echo)

