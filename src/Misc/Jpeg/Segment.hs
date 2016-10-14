module Misc.Jpeg.Segment
  ( Segment (..)
  , parse
  ) where

import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Lazy.Char8 as CS
import qualified Data.Binary.Get as Bin
import Data.ByteString.Char8 (unpack)
import Control.Monad (when)


data Segment
  = Soi     -- Start of Image
  | App0    -- App0
    { rev :: (Int, Int)
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

parseApp0 :: Bin.Get Segment
parseApp0 = do
  size <- Bin.getWord16be
  when (size < 16) $ fail "size must be larger than 15"
  str <- Bin.getByteString 5
  when (unpack str /= "JFIF\NUL") $ fail "invalid marker"
  return Eoi

parseSeg :: Bin.Get Segment
parseSeg = do
  idf <- Bin.getWord8
  when (idf /= 0xff) $ fail "Mark must be 0xff"
  marker <- Bin.getWord8
  case marker of
    0xd8 -> return Soi
    0xdb -> return Dqt
    0xc0 -> return Sof
    0xc4 -> return Dht
    0xda -> return Sos
    0xd9 -> return Eoi
    0xe0 -> parseApp0
    _ -> fail $ show marker


parse :: BS.ByteString -> Maybe [Segment]
parse blob =
    case Bin.runGetOrFail parseSeg blob of
      Left (_, _, c) -> Just []
      Right (rem, o, res) -> (res :) <$> parse rem
