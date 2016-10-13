module Core.Crypto (hashPassword) where

import qualified Data.ByteString.Internal         as BSI
import qualified Data.ByteString.Char8            as BS
import qualified Data.Bits                        as B
import qualified Foreign.Storable                 as FS
import qualified Foreign.Ptr                      as FP
import qualified Crypto.Hash.SHA256               as SHA256
import qualified Data.ByteString.Unsafe           as BSU
import Data.Bits ((.&.))


hashPassword :: BS.ByteString -> BS.ByteString
-- ^ Hash the given password
hashPassword = toHex . SHA256.hash
  where
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
    hexDigest d
        | d < 10 = d + 48
        | otherwise = d + 87
