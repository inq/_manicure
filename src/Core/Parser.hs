{-# LANGUAGE FlexibleContexts  #-}
module Core.Parser where

import qualified Data.ByteString.Lazy             as LS
import qualified Data.ByteString                  as BS
import qualified Data.Attoparsec.ByteString.Char8 as AC
import qualified Data.Attoparsec.ByteString.Lazy  as AL
import qualified GHC.Word                         as W
import Control.Monad (MonadPlus)
import Control.Applicative (Alternative)
import Prelude hiding (takeWhile)

type Parser = AC.Parser

peekChar' :: Parser Char
peekChar' = AC.peekChar'

skipSpace :: Parser ()
skipSpace = AC.skipSpace

token :: Char -> Parser W.Word8
token c = spaces *> char c <* spaces

isHorizontalSpace :: W.Word8 -> Bool
isHorizontalSpace = AC.isHorizontalSpace

count :: Monad m => Int -> m a -> m [a]
count = AC.count

digit :: Parser Char
digit = AC.digit

satisfy :: (Char -> Bool) -> Parser Char
satisfy = AC.satisfy

isToken :: W.Word8 -> Bool
isToken w = w <= 127
     && AL.notInClass "\0-\31()<>@,;:\\\"/[]?={} \t" w

anyChar :: Parser Char
anyChar = AC.anyChar

manyTill :: Alternative f => f a -> f b -> f [a]
manyTill = AC.manyTill

takeTill :: (W.Word8 -> Bool) -> Parser BS.ByteString
takeTill = AL.takeTill

takeWhile :: (W.Word8 -> Bool) -> Parser BS.ByteString
takeWhile = AL.takeWhile

skipWhile :: (W.Word8 -> Bool) -> Parser ()
skipWhile = AL.skipWhile

endOfLine :: Parser ()
endOfLine = AC.endOfLine

isEndOfLine :: W.Word8 -> Bool
isEndOfLine = AC.isEndOfLine

char :: Char -> Parser W.Word8
char = AC.char8

many :: MonadPlus m => m a -> m [a]
many = AL.many'

many1 :: Alternative f => f a -> f [a]
many1 = AL.many1

spaces :: Parser ()
spaces = AL.skipWhile AC.isHorizontalSpace

noneOf :: String -> Parser BS.ByteString
noneOf = takeWhile . AL.notInClass

noneOf1 :: String -> Parser BS.ByteString
noneOf1 = takeWhile . AL.notInClass

try :: Parser a -> Parser a
try = AC.try

sepBy :: Alternative f => f a -> f s -> f [a]
sepBy = AC.sepBy

string :: BS.ByteString -> Parser BS.ByteString
string = AC.string

parse :: Parser a -> LS.ByteString -> AL.Result a
parse = AL.parse

parseOnly :: Parser a -> BS.ByteString -> Either String a
parseOnly = AC.parseOnly

takeByteString :: Parser BS.ByteString
takeByteString = AC.takeByteString
