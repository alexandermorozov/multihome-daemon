import Control.Monad (liftM, forever)
import Data.Bits (complement, shiftR, shiftL, (.&.), (.|.))
import Data.Binary.Get (runGet, getWord8, getWord16be, remaining)
import Data.Binary.Put (runPut, putWord8, putWord16be, putWord32be, putLazyByteString)
import qualified Data.ByteString as SB
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Char8 as SB8
import Data.Word (Word8, Word16, Word32)
import Data.List (foldl')                        
import Network.BSD (getProtocolByName, protoNumber)
import Network.Socket (withSocketsDo, socket, inet_addr,
                       HostAddress, Family (AF_INET), SocketType (Raw),
                       SockAddr (SockAddrInet))
import Network.Socket.ByteString (sendTo, recvFrom)
import Text.Hexdump (hexdump)


data IpPacket = IpPacket {srcAddr :: Word32, 
                          dstAddr :: Word32, 
                          ipProtocol :: Word8, 
                          ipPayload :: B.ByteString
                         }
                
data IcmpPacket = IcmpPacket {icmpType :: Word8, 
                              icmpCode :: Word8,
                              icmpQuench :: Word32,
                              icmpPayload :: B.ByteString}

data IcmpEchoPacket = IcmpEchoPacket {echoId :: Word16, 
                                      echoSequence :: Word16,
                                      echoPayload :: B.ByteString}


main = withSocketsDo $ do
    addr <- inet_addr "192.168.3.2"
    doPing addr


doPing :: HostAddress -> IO ()
doPing addr = do
    proto <- getProtocolByName "icmp"
    s <- socket AF_INET Raw (protoNumber proto)
    let pkt = dumpIcmpEchoRequest $ IcmpEchoPacket 4902 4 B.empty
    sendTo s (unlazy pkt) (SockAddrInet 0 addr)
    forever $ do
        (p, saddr) <- recvFrom s 1024
        putStrLn $ hexdump 0 $ SB8.unpack p
  where
    unlazy = SB.concat . B.toChunks


--parseIp :: B.ByteString -> Maybe IpPacket


dumpIcmp :: IcmpPacket -> B.ByteString
dumpIcmp p =
    assemble $ inetChecksum $ assemble 0 
  where
    assemble csum = runPut $ do
        putWord8 $ icmpType p
        putWord8 $ icmpCode p
        putWord16be csum
        putWord32be $ icmpQuench p
        putLazyByteString $ icmpPayload p

-- dumps complete ICMP part of the packet
dumpIcmpEchoRequest :: IcmpEchoPacket -> B.ByteString
dumpIcmpEchoRequest p =
    let quench = (2^16 * fromIntegral (echoId p)) .|. fromIntegral (echoSequence p)
    in  dumpIcmp $ IcmpPacket 8 0 quench (echoPayload p)




-- Calculates standard IP checksum: one's compliment
inetChecksum :: B.ByteString -> Word16
inetChecksum packet = let s = foldl' (\a b -> compliment16 (a+b)) 0 $ split16 packet
                      in fromIntegral $ complement s
  where
    compliment16 x = let hi = shiftR 16 x
                         lo = x .&. 0xffff 
                     in if hi /= 0 then compliment16 (hi + lo) else lo
      
    split16 :: B.ByteString -> [Int]
    split16 = runGet split16helper 
    split16helper = do
        n <- remaining
        case n of
            0 -> return []
            1 -> do x <- liftM fromIntegral getWord8 
                    return [256 * x]
            _ -> do x <- liftM fromIntegral getWord16be 
                    rest <- split16helper
                    return (x:rest)

