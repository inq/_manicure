{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
module Core.Request where

import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as LS
import qualified Data.Char as C
import qualified Data.Map as M
import qualified Data.Attoparsec.ByteString.Lazy  as AL
import qualified Core.Http as Http
import qualified Misc.Parser as P
import Core.Request.Content (Content, mkContent)
import Network.Socket (Socket)
import Language.Haskell.TH.Syntax (Lift, lift)
import Misc.ByteString (QueryString, splitAndDecode)
import Network.Socket.ByteString (recv)
import Control.Monad (when)
import Control.Applicative (many)

-- * Data types

data Request = Request
  { method :: Method
  , version :: Http.Version
  , uri :: BS.ByteString
  , headers :: RequestHeaders
  , content :: Content
  , queryStr :: QueryString
  , requestSocket :: Socket
  } deriving (Show)

data Method = GET | POST | PUT | DELETE | PATCH
  | TRACE | OPTIONS | HEAD | CONNECT
  deriving (Show, Read, Eq, Ord)

instance Lift Method where
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

type Lines = ([BS.ByteString], BS.ByteString)

receiveHeader :: Socket -> IO Lines
-- ^ Receive header from the socket
receiveHeader fd = do
  buf <- recv fd 4096
  when (BS.length buf == 0) $ error "Disconnected"
  receiveHeader' [] buf
 where
  receiveHeader' res buffer = do
    let (line, remaining) = BS.breakSubstring "\r\n" buffer
    let remaining' = BS.drop 2 remaining
    if BS.length line == 0
      then return (res, remaining')
      else if BS.length remaining' == 0
        then do
          buf <- recv fd 4096
          if BS.length buf == 0
            then error "Disconnected"
            else receiveHeader' res $ BS.append remaining' buf
          else receiveHeader' (line : res) remaining'

extractCookie :: Request -> M.Map BS.ByteString BS.ByteString
-- ^ Extract cookie from the request header
extractCookie req =
    findCookie $ headers req
  where
    findCookie (("Cookie", context) : _) = splitAndDecode ';' context
    findCookie (_ : t)                   = findCookie t
    findCookie []                        = M.empty

parse :: LS.ByteString -> Socket -> Request
-- ^ Read and parse the data from socket to make the Request data
parse ipt = parseHead _head res content'
  where
    content' = mkContent (M.lookup "Content-Type" $ M.fromList res) $ LS.take contentLength pdata
    contentLength = case M.lookup "Content-Length" $ M.fromList res of
      Just len -> case BS.readInteger len of
        Just num -> fromIntegral $ fst num
        Nothing -> error "parse error"
      Nothing -> 0
    (_head, res, pdata) = case P.parse request ipt of
      AL.Done remaining (h, r)  -> (h, r, remaining)
      _ -> error "parse error"
    request = (,)
      <$> (P.takeTill P.isEndOfLine <* P.endOfLine)
      <*> (many header <* P.endOfLine)
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

parseHead :: BS.ByteString -> RequestHeaders -> Content -> Socket -> Request
-- ^ Parse the first line of the HTTP header
parseHead str _headers content' =
    Request _method _version _uri _headers content' queryString
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
    queryStringTail
      | BS.null queryStringRaw        = ""
      | BS.head queryStringRaw == '?' = BS.tail queryStringRaw
      | otherwise                     = ""
    queryString = splitAndDecode '&' queryStringTail
    _version = Http.Version
      (C.digitToInt $ BS.index str (_length - 3))
      (C.digitToInt $ BS.index str (_length - 1))
