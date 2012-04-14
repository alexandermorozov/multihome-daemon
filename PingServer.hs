--module PingServer where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.Chan (Chan, newChan, readChan, writeChan)
import Control.Monad (liftM, forever)
import qualified Data.ByteString as SB
import qualified Data.ByteString.Lazy as LB
import Network.BSD (getProtocolByName, protoNumber)
import Network.Socket (withSocketsDo, socket, inet_addr, Socket,
                       HostAddress, Family (AF_INET), SocketType (Raw),
                       SockAddr (SockAddrInet))
import Network.Socket.ByteString (sendTo, recvFrom)

import Icmp (IpPacket(..), IcmpEchoPacket(..),
             dumpIcmpEchoRequest, parseIp, parseIcmpEchoReply)

import Timer
import SockOpt

data PingServer = PingServer (Chan Command)

data Command = AddHost String
             | DelHost String
             | PingReceived IpPacket IcmpEchoPacket
             deriving(Show)


main = withSocketsDo $ do
    addr <- inet_addr "192.168.3.2"
    ps <- newPingServer "br0"
    forever $ threadDelay 1


newPingServer :: String -> IO PingServer
newPingServer interface = do
    chan <- newChan
    s <- openSocket interface
    forkIO $ startServer s chan
    return $ PingServer chan


openSocket :: String -> IO Socket
openSocket interface = do
    proto <- getProtocolByName "icmp"
    s <- socket AF_INET Raw (protoNumber proto)
    setSocketBindToDevice s interface
    return s
    
  
startServer :: Socket -> Chan Command -> IO ()
startServer s chan = do
    forkIO $ socketListener s chan
    loop
  where
    loop = do
        cmd <- readChan chan
        print cmd
        loop


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

