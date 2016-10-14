module Core.Request.Content where

import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as LS
import Misc.ByteString (QueryString, splitAndDecode)
import qualified Data.Map as M

-- * Data types

data Content = Text QueryString
  deriving Show

mkContent :: Maybe BS.ByteString -> LS.ByteString -> Content
mkContent contType cont = Text $ splitAndDecode '&' $ LS.toStrict cont

lookup :: BS.ByteString -> Content -> Maybe BS.ByteString
lookup key (Text cont) = M.lookup key cont
