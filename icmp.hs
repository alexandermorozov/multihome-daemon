import Control.Monad (liftM, liftM4, forever)
import Data.Bits (complement, shiftR, shiftL, (.&.), (.|.))
import Data.Binary.Get (runGet, getWord8, getWord16be, getWord32be,
                        getLazyByteString, getRemainingLazyByteString, remaining)
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
                         } deriving (Show)
                
data IcmpPacket = IcmpPacket {icmpType :: Word8, 
                              icmpCode :: Word8,
                              icmpQuench :: Word32,
                              icmpPayload :: B.ByteString
                             } deriving (Show)

data IcmpEchoPacket = IcmpEchoPacket {echoId :: Word16, 
                                      echoSequence :: Word16,
                                      echoPayload :: B.ByteString
                                     } deriving (Show)


main = withSocketsDo $ do
    addr <- inet_addr "192.168.3.2"
    doPing addr


doPing :: HostAddress -> IO ()
doPing addr = do
    proto <- getProtocolByName "icmp"
    s <- socket AF_INET Raw (protoNumber proto)
    let pkt = dumpIcmpEchoRequest $ IcmpEchoPacket 4902 4 B.empty
    sendTo s (fromLazy pkt) (SockAddrInet 0 addr)
    forever $ do
        (p, saddr) <- recvFrom s 1024
        -- putStrLn $ hexdump 0 $ SB8.unpack p
        let ipP = parseIp $ toLazy p 
        print ipP
        print $ parseIcmp (ipPayload ipP) 
        print $ parseIcmpEchoReply (ipPayload ipP) 
  where
    fromLazy = SB.concat . B.toChunks
    toLazy s = B.fromChunks [s]


parseIp :: B.ByteString -> IpPacket
parseIp = runGet $ do
    verAndIhl <- getWord8
    let ver = shiftR verAndIhl 4
    let ihl = verAndIhl .&. 0xf
    tos   <- getWord8
    len   <- getWord16be
    ident <- getWord16be
    flagsAndOffset <- getWord16be
    ttl   <- getWord8
    proto <- getWord8
    csum  <- getWord16be
    src   <- getWord32be
    dst   <- getWord32be
    options <- getLazyByteString $ fromIntegral ((ihl - 5) * 4)
    payload <- getRemainingLazyByteString
    return $ IpPacket src dst proto payload


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

parseIcmp :: B.ByteString -> IcmpPacket
parseIcmp = runGet $ do
    type'  <- getWord8 
    code   <- getWord8 
    csum   <- getWord16be
    quench <- getWord32be
    pld    <- getRemainingLazyByteString
    return $ IcmpPacket type' code quench pld


-- dumps complete ICMP part of the packet
dumpIcmpEchoRequest :: IcmpEchoPacket -> B.ByteString
dumpIcmpEchoRequest p =
    let quench = (2^16 * fromIntegral (echoId p)) .|. fromIntegral (echoSequence p)
    in  dumpIcmp $ IcmpPacket 8 0 quench (echoPayload p)

parseIcmpEchoReply :: B.ByteString -> IcmpEchoPacket
parseIcmpEchoReply b =
    let icmp  = parseIcmp b
        ident = fromIntegral $ shiftR (icmpQuench icmp) 16
        seq   = fromIntegral $ icmpQuench icmp .&. 0xffff
    in IcmpEchoPacket ident seq (icmpPayload icmp)

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

