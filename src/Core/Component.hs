{-# LANGUAGE OverloadedStrings #-}
module Core.Component where

import qualified Data.ByteString.Char8 as BS
import qualified Core.Database as DB
import qualified Core.Request as Req
import qualified Core.Response as Res
import qualified Database.MongoDB as Mongo
import qualified Database.Redis as R
import qualified Data.Map as M
import qualified Core.Request.Content as Content
import Control.Monad.State (StateT, runStateT, get, liftIO)
import Data.List (find)

-- * Data types

data ResState = ResState
  { conn    :: DB.Connection
  , params  :: [BS.ByteString]
  , req     :: Req.Request
  }

-- * Type Aliases

type Handler = StateT ResState IO Res.Response
type Component = StateT ResState IO BS.ByteString
type FlexComp a = StateT ResState IO a

-- * Handler

runHandler :: Handler -> [BS.ByteString] -> DB.Connection -> Req.Request
   -> IO (Res.Response, ResState)
runHandler c p n r = runStateT c (ResState n p r)

runDB :: Mongo.Action IO a -> StateT ResState IO a
-- ^ Run the DB action
runDB a = do
    n <- conn <$> get
    liftIO $ DB.query n a

runRedis :: R.Redis a -> StateT ResState IO a
-- ^ Run the Redis action
runRedis a = do
    n <- conn <$> get
    liftIO $ DB.runRedis n a

getCookie :: BS.ByteString -> StateT ResState IO (Maybe BS.ByteString)
-- ^ Read cookie from the state
getCookie key = M.lookup key . Req.extractCookie . req <$> get

getParams :: StateT ResState IO [BS.ByteString]
-- ^ Get parameters
getParams = params <$> get

postData :: BS.ByteString -> StateT ResState IO (Maybe Content.Context)
-- ^ Read Post variable
postData key = Content.lookup key . Req.content . req <$> get

postData' :: BS.ByteString -> StateT ResState IO BS.ByteString
-- ^ Read Post variable
postData' key = do
  res <- Content.lookup key . Req.content . req <$> get
  return $ case res of
    Just (Content.MkText bs) -> bs
    _ -> ""

requestHeader :: BS.ByteString -> StateT ResState IO (Maybe BS.ByteString)
-- ^ Read request header
requestHeader key = do
    headers <- Req.headers . req <$> get
    return (snd <$> find sel headers)
  where
    sel (k, _) = k == key

reqm :: StateT ResState IO BS.ByteString
-- ^ Read request header
reqm = do
    headers <- Req.headers . req <$> get
    return $ BS.pack $ show headers

reqn :: StateT ResState IO BS.ByteString
-- ^ Read request header
reqn = do
    headers <- Req.content . req <$> get
    return $ BS.pack $ show headers
