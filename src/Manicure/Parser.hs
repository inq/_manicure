{-# LANGUAGE FlexibleContexts  #-}
module Manicure.Parser where

import qualified Data.ByteString.Char8            as BS
import qualified Data.Attoparsec.ByteString       as AB
import qualified Data.Attoparsec.ByteString.Char8 as AC
import qualified GHC.Word                         as W
import Control.Monad (MonadPlus)
import Control.Applicative (Alternative)
import Prelude hiding (takeWhile)

type Parser = AC.Parser

token :: Char -> Parser (W.Word8)
token c = spaces *> char c <* spaces

isHorizontalSpace = AC.isHorizontalSpace

isToken :: W.Word8 -> Bool
isToken w = w <= 127
     && AB.notInClass "\0-\31()<>@,;:\\\"/[]?={} \t" w

takeTill :: (W.Word8 -> Bool) -> Parser BS.ByteString
takeTill = AB.takeTill

takeWhile :: (W.Word8 -> Bool) -> Parser BS.ByteString
takeWhile = AB.takeWhile

skipWhile = AB.skipWhile

endOfLine = AC.endOfLine

isEndOfLine :: W.Word8 -> Bool
isEndOfLine = AC.isEndOfLine

char :: Char -> Parser W.Word8
char = AC.char8

many :: MonadPlus m => m a -> m [a]
many = AC.many'

many1 :: Alternative f => f a -> f [a]
many1 = AC.many1

spaces = AB.skipWhile AC.isHorizontalSpace

noneOf :: String -> Parser BS.ByteString
noneOf = takeWhile . AB.notInClass

noneOf1 :: String -> Parser BS.ByteString
noneOf1 = takeWhile . AB.notInClass

try = AC.try

sepBy :: Alternative f => f a -> f s -> f [a]
sepBy = AC.sepBy

string = AC.string

parseOnly :: Parser a -> BS.ByteString -> Either String a
parseOnly = AC.parseOnly

takeByteString = AC.takeByteString
