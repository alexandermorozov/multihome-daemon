--module PingServer where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.Chan (Chan, newChan, readChan, writeChan)
import Control.Monad (liftM, forever)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State (StateT, execStateT, modify)
import qualified Data.ByteString as SB
import qualified Data.ByteString.Lazy as LB
import Data.Foldable (toList)
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

data PingServer = PingServer (Chan Command)

data PacketId = PacketId { pktIp :: IP
                         , pktId :: Int
                         , pktSeq :: Int
                         } deriving (Show, Eq)

data ServerState = ServerState { srvIps :: DQ.BankersDequeue IP
                               , srvTReel :: TimerReel
                               , srvSentPackets :: Map.Map PacketId UTCTime
                               , srvTestInterval :: Maybe Double
                               , srvTimeoutInterval :: Maybe Double
                               , srvLinkTimeout :: Maybe (IO ())
                               }

data Command = AddHost String
             | DelHost String
             | SetPingInterval Double
             | SetTimeoutInterval Double
             | Exit

             | StartPing
             | PingReceived IpPacket IcmpEchoPacket
             | PingTimedOut PacketId
             deriving(Show)

instance Ord PacketId where
    (PacketId a1 b1 c1) <= (PacketId a2 b2 c2) =
        (a1 <= a2) || (b1 <= b2) || (c1 <= c2)

main = withSocketsDo $ do
    addr <- inet_addr "192.168.3.2"
    ps <- newPingServer "br0"
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


runServer :: Socket -> Chan Command -> IO ()
runServer sock chan = do
    forkIO $ socketListener sock chan
    tReel <- newTimerReel
    let state = ServerState { srvIps = DQ.empty
                            , srvTReel = tReel
                            , srvSentPackets = Map.empty
                            , srvTestInterval = Nothing
                            , srvTimeoutInterval = Nothing
                            , srvLinkTimeout = Nothing
                            }
    execStateT loop state
    return ()
  where
    loop :: StateT ServerState IO ()
    loop = do
        cmd <- liftIO $ readChan chan
        liftIO $ print cmd
        case cmd of
            AddHost h -> addHost h >> loop
            DelHost h -> delHost h >> loop
            SetPingInterval dt ->
                modify (\s -> s {srvTestInterval = Just dt}) >> loop
            SetTimeoutInterval dt ->
                modify (\s -> s {srvTimeoutInterval = Just dt}) >> loop
            Exit -> return ()
            --StartPing -> startPing

    addHost h = do
        addr <- liftIO $ inet_addr h
        modify $ \s -> s {srvIps = DQ.pushFront (srvIps s) addr}
    delHost h = do
        addr <- liftIO $ inet_addr h
        let filterAddr = \as -> DQ.fromList $ filter (\a -> a /= addr) (toList as)
        modify $ \s -> s {srvIps = filterAddr $ srvIps s}


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

