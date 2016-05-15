{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
module Core.Response where

import qualified Data.ByteString.Char8          as BS
import qualified Core.Http                      as Http
import qualified Core.Request                   as Req
import qualified Core.Database                  as DB

type Handler = [BS.ByteString] -> Action
type Action = DB.Connection -> Req.Request -> IO Response

data Response = Response {
  version :: Http.Version,
  statusCode :: Int,
  cookies :: [BS.ByteString],
  content :: BS.ByteString
} deriving Show

instance Show (BS.ByteString -> Handler) where
    show _ = ""
instance Show Handler where
    show _ = ""

renderContent :: Response -> [BS.ByteString]
-- ^ Render the content
renderContent (Response _ _ _cookies _content) =
    map cookieToString _cookies ++
    [ "Content-Length: ", BS.pack $ show $ BS.length _content, "\r\n\r\n"
    , _content
    ]
  where
    cookieToString cookie = BS.concat ["Set-Cookie: ", cookie, "; path=/\r\n"]

render :: Response -> BS.ByteString
-- ^ Render to the ByteString
render r@(Response _ 200 _ _) =
    BS.concat ("HTTP/1.0 200 OK\r\n" : renderContent r)
render r@(Response _ 404 _ _) =
    BS.concat ("HTTP/1.0 404 Not Found\r\n" : renderContent r)
render (Response _ 303 _ url) =
    BS.concat
      [ "HTTP/1.0 303 See Other\r\n"
      , "Location: ", url, "\r\n\r\n"
      ]
render r@(Response _ _ _ _) =
    BS.concat ("HTTP/1.0 500 Internal Error\r\n" : renderContent r)

defaultVersion :: Http.Version
-- ^ The default version is HTTP 1.1
defaultVersion = Http.Version 1 1

success :: BS.ByteString -> [BS.ByteString] -> Response
-- ^ Generate a Response data which represents 200 OK
success bs _cookies = Response defaultVersion 200 _cookies bs

error :: Int -> BS.ByteString -> Response
-- ^ Error page
error code = Response defaultVersion code []

redirect :: BS.ByteString -> [BS.ByteString] -> Response
-- ^ Redirect to the specific URL
redirect url _cookies = Response defaultVersion 303 _cookies url
