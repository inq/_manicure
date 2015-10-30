{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
module Manicure.Response where

import qualified Language.Haskell.TH.Syntax     as TS
import qualified Data.ByteString.Char8          as BS
import qualified Manicure.Http                  as Http
import qualified Manicure.Request               as Req
import qualified Manicure.Database              as DB

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

render :: Response -> BS.ByteString
-- ^ Render to the ByteString
render (Response version 200 cookies content) =
    BS.concat (
      ["HTTP/1.0 200 OK\r\n"] ++ 
      map cookieToString cookies ++
      [
        "Content-Length: ", BS.pack $ show $ BS.length content,
        "\r\n\r\n", 
        content
      ])
  where
    cookieToString cookie = BS.concat ["Set-Cookie: ", cookie, "; path=/\r\n"]
render (Response version 303 cookies url) =
    BS.concat (
      ["HTTP/1.0 303 See Other\r\n",
        "Location: ", url,
        "\r\n\r\n"
      ])

defaultVersion :: Http.Version
-- ^ The default version is HTTP 1.1
defaultVersion = Http.Version 1 1

success :: BS.ByteString -> [BS.ByteString] -> Response
-- ^ Generate a Response data which represents 200 OK
success bs cookies = Response defaultVersion 200 cookies bs

redirect :: BS.ByteString -> Response
-- ^ Redirect to the specific URL
redirect url = Response defaultVersion 303 [] url
