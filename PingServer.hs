import Control.Monad (liftM, forever)
import Network.BSD (getProtocolByName, protoNumber)
import qualified Data.ByteString as SB
import qualified Data.ByteString.Lazy as LB
import Network.Socket (withSocketsDo, socket, inet_addr,
                       HostAddress, Family (AF_INET), SocketType (Raw),
                       SockAddr (SockAddrInet))
import Network.Socket.ByteString (sendTo, recvFrom)

import Icmp (IpPacket(..), IcmpEchoPacket(..),
             dumpIcmpEchoRequest, parseIp, parseIcmpEchoReply)


main = withSocketsDo $ do
    addr <- inet_addr "192.168.3.2"
    doPing addr


doPing :: HostAddress -> IO ()
doPing addr = do
    proto <- getProtocolByName "icmp"
    s <- socket AF_INET Raw (protoNumber proto)
    let pkt = dumpIcmpEchoRequest $ IcmpEchoPacket 4902 4 SB.empty
    sendTo s (fromLazy pkt) (SockAddrInet 0 addr)
    forever $ do
        (p, saddr) <- recvFrom s 1024
        -- putStrLn $ hexdump 0 $ SB8.unpack p
        case parseEcho p of
             Left errMsg -> print errMsg
             Right (ip, echo) -> print (ip, echo)
  where
    fromLazy = SB.concat . LB.toChunks
    parseEcho p = do
        ip <- parseIp p
        echo <- parseIcmpEchoReply $ ipPayload ip
        return (ip, echo)

