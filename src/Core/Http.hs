module Core.Http where

import qualified Network.Wreq                   as Wreq
import qualified Data.ByteString.Lazy.Char8     as BSL
import qualified Data.ByteString.Char8          as BS
import Control.Lens ((^.))

data Version = Version {
  major :: {-# UNPACK #-} !Int,
  minor :: {-# UNPACK #-} !Int
} deriving (Show)


fetch :: BS.ByteString -> IO BS.ByteString
fetch url = do
    response <- Wreq.get $ BS.unpack url
    return $ BS.concat $ BSL.toChunks (response ^. Wreq.responseBody)
