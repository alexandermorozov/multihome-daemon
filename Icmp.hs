module Icmp (IpPacket(..), IcmpPacket(..), IcmpEchoPacket(..),
             dumpIcmp, dumpIcmpEchoRequest, 
             parseIp, parseIcmp, parseIcmpEchoReply
            ) where

import Control.Monad (liftM)
import Data.Bits (complement, shiftR, shiftL, (.&.), (.|.))
import qualified Data.Binary.Get as LG
import Data.Binary.Strict.Get (runGet, getWord8, getWord16be, getWord32be,
                        getByteString, remaining)
import Data.Binary.Put (runPut, putWord8, putWord16be, putWord32be, putByteString)
import qualified Data.ByteString as SB
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString.Char8 as SB8
import Data.Word (Word8, Word16, Word32)
import Data.List (foldl')
import Text.Hexdump (hexdump)


data IpPacket = IpPacket {srcAddr :: Word32,
                          dstAddr :: Word32,
                          ipProtocol :: Word8,
                          ipPayload :: SB.ByteString
                         } deriving (Show)

data IcmpPacket = IcmpPacket {icmpType :: Word8,
                              icmpCode :: Word8,
                              icmpQuench :: Word32,
                              icmpPayload :: SB.ByteString
                             } deriving (Show)

data IcmpEchoPacket = IcmpEchoPacket {echoId :: Word16,
                                      echoSequence :: Word16,
                                      echoPayload :: SB.ByteString
                                     } deriving (Show)



parseIp :: SB.ByteString -> Either String IpPacket
parseIp p = fst $ flip runGet p $ do
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
    options <- getByteString $ fromIntegral ((ihl - 5) * 4)
    payload <- remaining >>= getByteString
    return $ IpPacket src dst proto payload


dumpIcmp :: IcmpPacket -> LB.ByteString
dumpIcmp p =
    assemble $ inetChecksum $ assemble 0
  where
    assemble csum = runPut $ do
        putWord8 $ icmpType p
        putWord8 $ icmpCode p
        putWord16be csum
        putWord32be $ icmpQuench p
        putByteString $ icmpPayload p

parseIcmp :: SB.ByteString -> Either String IcmpPacket
parseIcmp p = fst $ flip runGet p $ do
    type'  <- getWord8
    code   <- getWord8
    csum   <- getWord16be
    quench <- getWord32be
    pld    <- remaining >>= getByteString
    return $ IcmpPacket type' code quench pld


-- dumps complete ICMP part of the packet
dumpIcmpEchoRequest :: IcmpEchoPacket -> LB.ByteString
dumpIcmpEchoRequest p =
    let quench = (2^16 * fromIntegral (echoId p)) .|. fromIntegral (echoSequence p)
    in  dumpIcmp $ IcmpPacket 8 0 quench (echoPayload p)

parseIcmpEchoReply :: SB.ByteString -> Either String IcmpEchoPacket
parseIcmpEchoReply b = do
    icmp <- parseIcmp b
    let ident = fromIntegral $ shiftR (icmpQuench icmp) 16
        seq   = fromIntegral $ icmpQuench icmp .&. 0xffff
    return $ IcmpEchoPacket ident seq (icmpPayload icmp)

-- Calculates standard IP checksum: one's compliment
inetChecksum :: LB.ByteString -> Word16
inetChecksum packet = let s = foldl' (\a b -> compliment16 (a+b)) 0 $ split16 packet
                      in fromIntegral $ complement s
  where
    compliment16 x = let hi = shiftR 16 x
                         lo = x .&. 0xffff
                     in if hi /= 0 then compliment16 (hi + lo) else lo

    split16 :: LB.ByteString -> [Int]
    split16 = LG.runGet split16helper
    split16helper = do
        n <- LG.remaining
        case n of
            0 -> return []
            1 -> do x <- liftM fromIntegral LG.getWord8
                    return [256 * x]
            _ -> do x <- liftM fromIntegral LG.getWord16be
                    rest <- split16helper
                    return (x:rest)

