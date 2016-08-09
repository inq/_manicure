{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
module Core.Response where

import qualified Data.ByteString.Char8          as BS
import qualified Core.Http                      as Http

-- * Data types

data Response = Response
  { version :: {-# UNPACK #-} !Http.Version
  , statusCode :: {-# UNPACK #-} !Int
  , cookies :: [BS.ByteString]
  , content :: {-# UNPACK #-} !BS.ByteString
  } deriving Show


cookieToString :: [BS.ByteString] -> [BS.ByteString]
-- ^ Convert cookies to string
cookieToString = map (\x -> BS.concat ["Set-Cookie: ", x, "; path=/\r\n"])

renderContent :: Response -> [BS.ByteString]
-- ^ Render the content
renderContent (Response _ _ _cookies _content) =
    cookieToString _cookies ++
    [ "Content-Length: ", BS.pack $ show $ BS.length _content, "\r\n\r\n"
    , _content
    ]

render :: Response -> BS.ByteString
-- ^ Render to the ByteString
render r@(Response _ 200 _ _) =
    BS.concat ("HTTP/1.0 200 OK\r\n" : renderContent r)
render r@(Response _ 404 _ _) =
    BS.concat ("HTTP/1.0 404 Not Found\r\n" : renderContent r)
render (Response _ 303 _cookies url) =
    BS.concat $
      [ "HTTP/1.0 303 See Other\r\n" ] ++
      cookieToString _cookies ++
      [ "Location: ", url, "\r\n\r\n"]
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
