{-# LANGUAGE OverloadedStrings #-}
module Core.Handler where

import qualified Data.ByteString.Char8          as BS
import qualified Core.Database                  as DB
import qualified Core.Request                   as Req
import qualified Control.Monad.State            as MS
import qualified Core.Response                  as Res

-- * Data types

data ResState = ResState
  { getConn    :: DB.Connection
  , getParams  :: [BS.ByteString]
  , getReq     :: Req.Request
  }

-- * Type Aliases

type Handler = MS.StateT ResState IO Res.Response
type Component = MS.StateT ResState IO [BS.ByteString]

-- * Handler

runHandler :: Handler -> [BS.ByteString] -> DB.Connection -> Req.Request
   -> IO (Res.Response, ResState)
runHandler code params conn req = MS.runStateT code (ResState conn params req)
