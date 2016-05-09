{-# LANGUAGE FlexibleInstances #-}
module Core.ByteString where

import qualified Data.ByteString.Char8          as BS
import qualified Data.Map                       as M
import qualified Network.HTTP.Types.URI         as URI
import qualified Data.ByteString.UTF8           as UTF8

class StringFamily a where
    convert :: a -> BS.ByteString
instance StringFamily BS.ByteString where
    convert bs = bs
instance StringFamily String where
    convert str = UTF8.fromString str

type QueryString = M.Map BS.ByteString BS.ByteString

splitAndDecode :: Char -> BS.ByteString -> QueryString
-- ^ Split the given string and construct the Map
splitAndDecode and bs = M.fromList $ map transform (BS.split and bs)
  where
    transform line = pair
      where
        idx = case BS.elemIndex '=' line of
            Just i -> i
            Nothing -> 0
        pair = (decode $ BS.take idx line, decode $ BS.drop (idx + 1) line)
          where
            decode = URI.urlDecode True
