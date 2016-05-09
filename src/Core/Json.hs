{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE EmptyCase         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
module Core.Json where

import qualified Data.ByteString.Char8            as BS
import qualified Data.ByteString.UTF8             as UTF8
import qualified Data.Attoparsec.ByteString       as AB
import qualified Data.Attoparsec.ByteString.Char8 as AC
import qualified Data.Map.Strict                  as M
import qualified Data.Time.Clock                  as TC
import Database.MongoDB (ObjectId)
import Data.Char (chr)
import Control.Applicative ((*>), (<*), (<$>), (<*>), (<|>))
import Control.Monad

data Json = JSString  BS.ByteString
    | JSInt Int
    | JSObject (M.Map BS.ByteString Json)
    | JSArray [Json]
    | JSBoolean Bool
    | JSObjectId BS.ByteString
    | JSNil
    deriving (Eq, Show)

class ToJson a where
-- ^ JSON transformation
    toJson :: a -> Json
instance ToJson TC.UTCTime where
    toJson = JSString . BS.pack . show 
instance ToJson ObjectId where
    toJson = JSObjectId . BS.pack . show
instance ToJson Integer where
    toJson = JSInt . fromIntegral
instance ToJson BS.ByteString where
    toJson = JSString
instance ToJson [Char] where
    toJson = JSString . BS.pack
instance ToJson t => ToJson (Maybe t) where
    toJson (Just a) = toJson a
    toJson Nothing  = JSNil

parse :: BS.ByteString -> Json
-- ^ Parse the given bytestring
parse str = case AC.parseOnly parseJson str of
    Right val -> val
    Left err -> undefined

parseJson :: AB.Parser Json
-- ^ The actual parser
parseJson = parseObject <|> parseArray <|> parseString <|> parseBoolean <|> parseInt
  where
    parseString = liftM (JSString . UTF8.fromString) $ op '"' *> AC.manyTill anyChar (AC.char8 '"')
      where
        escapeMap = M.fromList [
            ('"',  '"'),
            ('\\', '\\'),
            ('/',  '/'),
            ('n',  '\n'), 
            ('r',  '\r'),
            ('f',  '\f'),
            ('t',  '\t'),
            ('b',  '\b')
          ]
        isHexDigit w = (w >= '0' && w <= '9') ||
                       (w >= 'A' && w <= 'F') ||
                       (w >= 'a' && w <= 'f')
        anyChar = escaped <|> AC.anyChar
        escaped = do
            AC.char8 '\\'
            c <- AC.anyChar
            case M.lookup c escapeMap of
                Just ch -> return ch
                Nothing -> if c == 'u'
                    then do 
                        code <- AC.count 4 (AC.satisfy isHexDigit)
                        if null code
                             then fail "broken unicode"
                             else return $ chr $ read $ "0x" ++ code
                    else fail "broken escape character"
        
    spaces = AB.skipWhile AC.isHorizontalSpace
    parseArray = liftM JSArray $ op '[' *> AC.sepBy (spaces *> parseJson <* spaces) (AC.char8 ',') <* cl ']'
    parseObject = liftM (JSObject . M.fromList) $ op '{' *> AC.sepBy pair (AC.char8 ',') <* cl '}'
      where
        pair = do
            key <- spaces *> parseString <* spaces <* AC.char8 ':'
            value <- spaces *> parseJson <* spaces 
            return (rawString key, value)
        rawString (JSString x) = x
    parseBoolean = liftM JSBoolean (parseTrue <|> parseFalse)
      where
        parseTrue = do AC.try (spaces *> AC.string "true" *> spaces); return True
        parseFalse = do AC.try (spaces *> AC.string "false" *> spaces); return False
    parseInt = liftM (JSInt . read) $ AC.try (AC.many1 AC.digit)
    op c = AC.try (spaces *> AC.char8 c)
    cl c = AC.char c <* spaces
