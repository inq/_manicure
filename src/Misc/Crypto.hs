module Misc.Crypto (hashPassword) where

import qualified Data.ByteString.Char8 as BS
import qualified Crypto.Hash.SHA256 as SHA256
import Foreign.Ptr (plusPtr)
import Foreign.Storable (poke)
import Data.ByteString.Unsafe (unsafeIndex)
import Data.ByteString.Internal (unsafeCreate)
import Data.Bits (shiftR, (.&.))


hashPassword :: BS.ByteString -> BS.ByteString
-- ^ Hash the given password
hashPassword = toHex . SHA256.hash
  where
    toHex bs = unsafeCreate nl $ go 0
      where
        len = BS.length bs
        nl = 2 * len
        go i p
          | i == len  = return ()
          | otherwise = case unsafeIndex bs i of
            w -> do
              poke p (hexDigest $ w `shiftR` 4)
              poke (p `plusPtr` 1) (hexDigest $ w .&. 0xF)
              go (i + 1) (p `plusPtr` 2)
    hexDigest d
      | d < 10 = d + 48
      | otherwise = d + 87
