import Control.Monad (liftM)
import Data.Bits (complement, shiftR, shiftL, (.&.), (.|.))
import Data.Binary.Get (runGet, getWord8, getWord16be, remaining)
import Data.Binary.Put (runPut, putWord8, putWord16be, putWord32be, putLazyByteString)
import qualified Data.ByteString as SB
import qualified Data.ByteString.Lazy as B
import Data.Word (Word8, Word16, Word32)
import Data.List (foldl')                        
import Network.BSD (getProtocolByName, protoNumber)
import Network.Socket (withSocketsDo, socket, inet_addr,
                       HostAddress, Family (AF_INET), SocketType (Raw),
                       SockAddr (SockAddrInet))
import Network.Socket.ByteString (sendTo)

main = withSocketsDo $ do
    addr <- inet_addr "192.168.3.2"
    doPing addr


doPing :: HostAddress -> IO ()
doPing addr = do
    proto <- getProtocolByName "icmp"
    s <- socket AF_INET Raw (protoNumber proto)
    let pkt = createIcmpEchoRequest 4902 4 B.empty
    sendTo s (unlazy pkt) (SockAddrInet 0 addr)
    return ()
  where
    unlazy = SB.concat . B.toChunks

createIcmpEchoRequest :: Word16 -> Word16 -> B.ByteString -> B.ByteString
createIcmpEchoRequest identifier sequence payload =
    let quench = (2^16 * (fromIntegral identifier)) .|. fromIntegral sequence
    in  createIcmp 8 0 quench payload

createIcmp :: Word8 -> Word8 -> Word32 -> B.ByteString -> B.ByteString
createIcmp type' code quench payload =
    assemble $ inetChecksum $ assemble 0 
  where
    assemble csum = runPut $ do
        putWord8 type'
        putWord8 code
        putWord16be csum
        putWord32be quench
        putLazyByteString payload 

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

