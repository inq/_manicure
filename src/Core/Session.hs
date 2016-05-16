{-# LANGUAGE OverloadedStrings #-}
module Core.Session where

import qualified Crypto.Hash.SHA256               as SHA256
import qualified Data.Time.Clock.POSIX            as POSIX
import qualified Data.ByteString.Char8            as BS
import qualified Core.Database                    as DB
import qualified Foreign.Ptr                      as FP
import qualified Foreign.Storable                 as FS
import qualified Data.ByteString.Unsafe           as BSU
import qualified Data.ByteString.Internal         as BSI
import qualified Data.Bits                        as B
import Data.Bits ((.&.))

generateKey :: IO BS.ByteString
-- ^ Generate a session key
generateKey = do
    t <- POSIX.getPOSIXTime
    return $ toHex . SHA256.hash . BS.pack $ show t
  where
    hexDigest d
        | d < 10 = d + 48
        | otherwise = d + 87
    toHex bs = BSI.unsafeCreate nl $ go 0
      where
        len = BS.length bs
        nl = 2 * len
        go i p
          | i == len  = return ()
          | otherwise = case BSU.unsafeIndex bs i of
              w -> do 
                  FS.poke p (hexDigest $ w `B.shiftR` 4)
                  FS.poke (p `FP.plusPtr` 1) (hexDigest $ w .&. 0xF)
                  go (i + 1) (p `FP.plusPtr` 2)

mkSession :: IO BS.ByteString
-- ^ Make a new session data
mkSession = generateKey

readSession :: DB.Connection -> BS.ByteString -> IO BS.ByteString
readSession = DB.redisGet
