import qualified Data.ByteString.Char8          as BS
import qualified Core.Database                  as DB
import qualified Core.Request                   as Req
import qualified Control.Monad.State            as MS

data ResState = ResState
  { getConn    :: DB.Connection
  , getParams  :: [BS.ByteString]
  , getReq     :: Req.Request
  }

type Handler = MS.StateT ResState IO Response
type Component = MS.StateT ResState IO [BS.ByteString]

runAction :: Handler -> [BS.ByteString] -> DB.Connection -> Req.Request
   -> IO (Response, ResState)
runAction code params conn req = MS.runStateT code (ResState conn params req)
