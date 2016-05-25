{-# LANGUAGE OverloadedStrings #-}
module Core.Buffer where

import qualified Network.Socket                 as NS
import qualified Network.Socket.ByteString      as NSB 
import qualified Data.ByteString.Char8          as BS

data Buffer = Buffer !NS.Socket !BS.ByteString

fromSocket :: NS.Socket -> IO Buffer
-- ^ Receive the first chunk and construct a buffer
fromSocket fd = Buffer fd <$> NSB.recv fd 4096

readLine :: Buffer -> IO (Buffer, BS.ByteString)
-- ^ Read a line from the buffer
readLine (Buffer fd buf) = do
    let (line, remaining') = BS.breakSubstring "\r\n" buf
    let remaining = BS.drop 2 remaining'
    putStrLn "readLin!!!!!!!!!!!"
    BS.putStrLn line
    if BS.length remaining' == 0
        then do
            buf' <- NSB.recv fd 4096
            if BS.length buf' == 0
                then error "Disconnected"
                else readLine $ Buffer fd $ BS.append remaining buf'
        else return (Buffer fd remaining', line)
