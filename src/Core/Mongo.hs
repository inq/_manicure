module Core.Mongo where

import qualified Data.IORef     as IR
import qualified Network        as N
import qualified Data.ByteString as BS
import qualified Control.Concurrent.MVar.Lifted as CM
import qualified System.IO as IO
import qualified System.IO.Unsafe as IOU

data Host = Host N.HostName N.PortID 

data Transport = Transport
  { read :: Int -> IO BS.ByteString
  , write :: BS.ByteString -> IO ()
  , flust :: IO ()
  , close :: IO ()
  }

data Pipe = Pipe 
  { vStream :: CM.MVar Transport
  , responsQueue :: Chan (CM.MVar (Either IOError Response))
  , listenThread :: ThreadId
  }

fromHandle handle = do
  return Transport (BS.hGet handle) (BS.hPut handle) (IO.hFlush handle) (IO.hClose handle)


connect :: Host -> IO Pipe
connect h = do
    sec <- IR.readIORef $ IOU.unsafePerformIO $ IR.newIORef 6 
    connect' h sec

connect' :: Host -> Double -> IO Pipe
connect' (Host hostname port) timeoutSecs = do

