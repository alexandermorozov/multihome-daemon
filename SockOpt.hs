{-# LANGUAGE ForeignFunctionInterface #-}
module SockOpt (setSocketBindToDevice) where

import Control.Exception.Base
import Foreign
import Foreign.C.Types
import Foreign.C.String
import Network.Socket

foreign import ccall "sys/socket.h setsockopt"
     c_setSockOpt :: CInt -> CInt -> CInt -> CString -> CInt -> IO CInt

-- Debian 7.0; /usr/include/asm-generic/socket.h
soBindToDevice = 25

setSockOptStr :: Socket -> Int -> Int -> String -> IO Int
setSockOptStr sock level optname valStr = do
    let MkSocket sId _ _ _ _ = sock
    (valC, valLen) <- newCStringLen valStr
    ret <- c_setSockOpt sId (fromIntegral level) (fromIntegral optname) 
                        valC (fromIntegral valLen)
    return $ fromIntegral ret  

setSocketBindToDevice :: Socket -> String -> IO ()
setSocketBindToDevice sock iface = do
    ret <- setSockOptStr sock sOL_SOCKET soBindToDevice (iface ++ "\0")
    if ret /= 0
        then let msg = "Cannot bind to device " ++ iface ++": " ++ show(ret)
             in  ioError $ userError msg
        else return ()

