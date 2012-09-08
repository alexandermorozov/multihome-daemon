--module PingServer where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar
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

data PacketId = PacketId { pktIp  :: IP
                         , pktId  :: Int
                         , pktSeq :: Int
                         } deriving (Show, Eq)

data PingServer = PingServer
    { srvCmd   :: MVar Command
    , srvTReel :: TimerReel

    , srvIps             :: [IP]
    , srvPingInterval    :: Double
    , srvTimeoutInterval :: Double
    , srvDownAction      :: IO ()
    , srvUpAction        :: IO ()
    }



data Command = Exit
             | DoPing
             | PingReceived IpPacket IcmpEchoPacket
             | PingTimedOut PacketId
             | LinkTimeout


instance Ord PacketId where
    (PacketId a1 b1 c1) <= (PacketId a2 b2 c2) =
        (a1 <= a2) || (b1 <= b2) || (c1 <= c2)

main = withSocketsDo $ do
    ps <- startPingServer "br0" ["192.168.3.2"] 2 10 (print "down!") (print "up!")
    forever $ threadDelay 1


startPingServer :: String -> [String] -> Double -> Double ->
                 IO () -> IO () -> IO PingServer
startPingServer interface ips interval timeout downAct upAct = do
    cmdTMV <- newEmptyMVar
    tReel  <- startTimerReel
    s      <- openSocket interface
    parsedIps <- mapM inet_addr ips
    let srv = PingServer cmdTMV tReel parsedIps interval timeout downAct upAct
    forkIO $ runServer s srv
    return srv
  where
    openSocket interface = do
        proto <- getProtocolByName "icmp"
        s <- socket AF_INET Raw (protoNumber proto)
        setSocketBindToDevice s interface
        return s


runServer :: Socket -> PingServer -> IO ()
runServer sock server = do
    send DoPing
    forkIO $ socketListener sock send
    loop (DQ.fromList $ srvIps server)
  where
    send = putMVar (srvCmd server)
    loop :: DQ.BankersDequeue IP -> IO ()
    loop ips = do
        cmd <- takeMVar (srvCmd server)
        case cmd of
            DoPing -> do
                print "Start ping"
                addTimer (srvTReel server) (srvPingInterval server) (send DoPing)
                let echo = dumpIcmpEchoRequest $ IcmpEchoPacket 10 10 SB.empty
                let ip = (fromJust $ DQ.first $ ips)
                sendTo sock (toStrictBS echo) (SockAddrInet 0 ip)
                loop (DQ.pushBack ips ip)
            PingReceived ip echo -> do
                print $ "Got ping from " ++ show(ip) ++ " " ++ show(echo)
                loop ips
            LinkTimeout -> do
                print "Link timeout"
                loop ips
            Exit -> return () -- TODO: kill listener and reel
            --StartPing -> startPing
    filterAddr addr addrs =  DQ.fromList $ filter (/= addr) (toList addrs)
    toStrictBS = SB.concat . LB.toChunks


socketListener :: Socket -> (Command -> IO()) -> IO ()
socketListener s send = do
    forever $ do
        (p, saddr) <- recvFrom s 65536
        case parseEcho p of
             Left errMsg -> print errMsg
             Right (ip, echo) -> send $ PingReceived ip echo
  where
    parseEcho p = do
        ip <- parseIp p
        echo <- parseIcmpEchoReply $ ipPayload ip
        return (ip, echo)

