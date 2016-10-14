module Misc.Jpeg.Segment
  ( Segment (..)
  , parse
  ) where

import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Lazy.Char8 as CS
import Data.Char( ord )


data Segment
  = Soi     -- Start of Image
  | App0    -- App0
    { idf :: String
    , rev :: (Int, Int)
    , unit :: Int
    , density :: (Int, Int)
    , thumbSize :: (Int, Int)
    }
  | App Int -- Application segments
  | Dqt     -- Quantization Table
  | Sof     -- Start of Frame
  | Dht     -- Huffman Table
  | Sos     -- Start of Scan
  | Eoi     -- End of Image
  deriving Show


parse :: BS.ByteString -> Maybe [Segment]
parse blob = do
    (mk, skip) <- marker m
    case skip of
      0 -> Just []
      _ -> (mk :) <$> parse (BS.drop skip blob)
  where
    chr i = fromIntegral $ BS.index blob i
    m = BS.index blob 1
    sh = chr 2
    sl = chr 3
    marker c = case c of
      0xd8 -> Just (Soi, 2)
      0xdb -> Just (Dqt, 0)
      0xc0 -> Just (Sof, 0)
      0xc4 -> Just (Dht, 0)
      0xda -> Just (Sos, 0)
      0xd9 -> Just (Eoi, 0)
      0xe0 -> readApp0
      _ ->
        if c >= 0xe1 && c <= 0xef
          then Just (App (fromIntegral c - 0xe0), 2)
          else Just (Eoi, 0)
    readApp0 = Just (App0
      { idf = CS.unpack $ BS.drop 4 $ BS.take 9 blob
      , rev =
        ( chr 9
        , chr 10
        )
      , unit = chr 11
      , density =
        ( chr 12 * 25 + chr 13
        , chr 14 * 25 + chr 15
        )
      , thumbSize =
        ( chr 16
        , chr 17
        )
      }, 2)
