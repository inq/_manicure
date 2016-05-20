{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes     #-}
module Core.Request where

import qualified Language.Haskell.TH.Syntax       as TS
import qualified Data.ByteString.Char8            as BS
import qualified Core.Http                        as Http
import qualified Network.Socket                   as NS
import qualified Data.Char                        as C
import qualified Data.Map                         as M
import qualified Core.ByteString                  as ByteString
import qualified Core.Parser                      as P
import Control.Applicative (many)

data Request = Request {
  method    :: Method,
  version   :: Http.Version,
  uri       :: BS.ByteString,
  headers   :: RequestHeaders,
  post      :: ByteString.QueryString,
  queryStr  :: ByteString.QueryString,
  requestSocket :: NS.Socket
} deriving (Show)

data Method = GET | POST | PUT | DELETE | PATCH
  | TRACE | OPTIONS | HEAD | CONNECT
  deriving (Show, Read, Eq, Ord)

instance TS.Lift Method where
    lift GET     = [| GET     |]
    lift POST    = [| POST    |]
    lift PUT     = [| PUT     |]
    lift DELETE  = [| DELETE  |]
    lift PATCH   = [| PATCH   |]
    lift TRACE   = [| TRACE   |]
    lift OPTIONS = [| OPTIONS |]
    lift HEAD    = [| HEAD    |]
    lift CONNECT = [| CONNECT |]

type RequestHeaders = [Header]
type Header = (BS.ByteString, BS.ByteString)

extractCookie :: Request -> M.Map BS.ByteString BS.ByteString
-- ^ Extract cookie from the request header
extractCookie req = 
    findCookie $ headers req
  where
    findCookie (("Cookie", context) : _) = ByteString.splitAndDecode ';' context
    findCookie (_ : t)                   = findCookie t
    findCookie []                        = M.empty

parse :: BS.ByteString -> NS.Socket -> Request
-- ^ Read and parse the data from socket to make the Request data
parse ipt = parseHead _head res _post 
  where 
    _post  = ByteString.splitAndDecode '&' pdata
    (_head, res, pdata) = case P.parseOnly request ipt of
        Right _res -> _res
        Left  str -> error str
    request = (,,)
        <$> (P.takeTill P.isEndOfLine <* P.endOfLine)
        <*> many header 
        <*> (P.endOfLine *> P.takeByteString)
    header = (,)
        <$> (P.takeWhile P.isToken <* P.char ':' <* P.skipWhile P.isHorizontalSpace)
        <*> (P.takeTill P.isEndOfLine <* P.endOfLine)
    
splitLines :: BS.ByteString -> [BS.ByteString]
-- ^ Split the lines from the HTTP header
splitLines str =
    case BS.elemIndex '\r' str of
        Just i | i > 2 -> BS.take i str : splitLines (BS.drop (i + 2) str)
        Just _         -> [BS.drop 2 str]
        Nothing        -> [""]

        
parseHead :: BS.ByteString -> RequestHeaders -> ByteString.QueryString -> NS.Socket -> Request
-- ^ Parse the first line of the HTTP header
parseHead str _headers query =
    Request _method _version _uri _headers query queryString
  where
    _method = case BS.index str 0 of
        'G' -> GET
        'D' -> DELETE
        'C' -> CONNECT
        _   -> case BS.index str 1 of
            'O' -> POST
            'U' -> PUT
            'A' -> PATCH
            'P' -> OPTIONS
            'E' -> HEAD
            _   -> TRACE
    _length = BS.length str
    uriLong = BS.drop (offset _method) $ BS.take (_length - 9) str
      where
        offset :: Method -> Int
        offset GET     = 4
        offset POST    = 5
        offset PUT     = 4
        offset HEAD    = 5
        offset OPTIONS = 7
        offset CONNECT = 7
        offset _       = 6
    (_uri, queryStringRaw) = BS.break (== '?') uriLong
    queryStringTail | BS.null queryStringRaw        = ""
                    | BS.head queryStringRaw == '?' = BS.tail queryStringRaw
                    | otherwise                     = ""
    queryString = ByteString.splitAndDecode '&' queryStringTail
    _version = Http.Version 
        (C.digitToInt $ BS.index str (_length - 3)) 
        (C.digitToInt $ BS.index str (_length - 1))
