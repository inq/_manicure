{-# LANGUAGE OverloadedStrings #-}
module Core.Component where

import qualified Data.ByteString.Char8          as BS
import qualified Core.Database                  as DB
import qualified Core.Request                   as Req
import qualified Control.Monad.State            as MS
import qualified Core.Response                  as Res
import qualified Database.MongoDB               as Mongo
import qualified Data.Map                       as M
import qualified Database.Redis                 as R
import Data.Maybe (fromJust)

-- * Data types

data ResState = ResState
  { conn    :: DB.Connection
  , params  :: [BS.ByteString]
  , req     :: Req.Request
  }

-- * Type Aliases

type Handler = MS.StateT ResState IO Res.Response
type Component = MS.StateT ResState IO [BS.ByteString]

-- * Handler

runHandler :: Handler -> [BS.ByteString] -> DB.Connection -> Req.Request
   -> IO (Res.Response, ResState)
runHandler c p n r = MS.runStateT c (ResState n p r)

runDB :: Mongo.Action IO a -> MS.StateT ResState IO a
-- ^ Run the DB action
runDB a = do
    n <- conn <$> MS.get
    MS.liftIO $ DB.query n a

runRedis :: R.Redis a -> MS.StateT ResState IO a
-- ^ Run the Redis action
runRedis a = do
    n <- conn <$> MS.get
    MS.liftIO $ DB.runRedis n a

getCookie :: BS.ByteString -> MS.StateT ResState IO (Maybe BS.ByteString)
-- ^ Read cookie from the state
getCookie key = M.lookup key . Req.extractCookie . req <$> MS.get

getParams :: MS.StateT ResState IO [BS.ByteString]
-- ^ Get parameters
getParams = params <$> MS.get

postData :: BS.ByteString -> MS.StateT ResState IO (Maybe BS.ByteString)
-- ^ Read Post variable
postData key = M.lookup key . Req.post . req <$> MS.get

postData' :: BS.ByteString -> MS.StateT ResState IO BS.ByteString
-- ^ Read Post variable
postData' key = fromJust . M.lookup key . Req.post . req <$> MS.get
