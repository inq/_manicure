module Core.Launcher where

import qualified Network                        as N
import qualified Network.Socket                 as NS
import qualified Network.Socket.ByteString      as NSB 
import qualified Core.Database                  as DB
import qualified Control.Concurrent             as CC
import qualified System.Posix.Process           as P
import qualified System.Posix.Files             as PF
import qualified System.IO                      as IO
import qualified System.Posix.IO                as PIO
import qualified System.Posix.Signals           as PS
import qualified System.Directory               as D
import qualified Core.Route                     as Route
import qualified Core.Request                   as Req
import qualified Core.Response                  as Res
import qualified Data.Text                      as T
import qualified Control.Monad                  as M

daemonize :: [Char] -> [Char] -> [Char] -> IO () -> IO ()
-- ^ Daemonize the given function
daemonize pidFile stdOut stdErr process = do
    exists <- D.doesFileExist pidFile
    M.when exists $ removeAndKill pidFile
    ignore $ P.forkProcess $ do
        P.createSession
        ignore $ P.forkProcess $ do
            writePid pidFile
            remapFds
            process
  where
    ignore f = f >> return ()
    remapFds = do
        devNull <- PIO.openFd "/dev/null" PIO.ReadOnly Nothing PIO.defaultFileFlags
        _ <- PIO.dupTo devNull PIO.stdInput
        PIO.closeFd devNull
        fd <- PIO.openFd stdOut PIO.ReadWrite (Just PF.stdFileMode) PIO.defaultFileFlags
        PIO.dupTo fd PIO.stdOutput
        PIO.closeFd fd
        fd <- PIO.openFd stdErr PIO.ReadWrite (Just PF.stdFileMode) PIO.defaultFileFlags
        PIO.dupTo fd PIO.stdError
        PIO.closeFd fd
    writePid pidFile = do
        fd <- PIO.createFile pidFile PF.stdFileMode
        pid <- P.getProcessID
        putStrLn $ show pid
        PIO.fdWrite fd (show pid)
    removeAndKill pidFile = do
        pid <- readFile pidFile
        D.removeFile pidFile
        putStrLn ("pid file exists: " ++ pid)
        PS.signalProcess PS.sigQUIT $ read pid

run :: Route.RouteTree -> T.Text -> [Char] -> IO ()
-- ^ Run the given RouteTree
run routeTree databaseName socketFile = N.withSocketsDo $ do
    removeExistingSocket socketFile
    db <- DB.connect databaseName
    socketFd <- NS.socket NS.AF_UNIX NS.Stream 0
    NS.bind socketFd $ NS.SockAddrUnix socketFile
    NS.listen socketFd 10
    PF.setFileMode socketFile PF.stdFileMode
    acceptSocket routeTree socketFd db
  where
    removeExistingSocket socketFile = do
      exists <- D.doesFileExist socketFile
      M.when exists $ D.removeFile socketFile

acceptSocket :: Route.RouteTree -> NS.Socket -> DB.Connection -> IO ()
-- ^ Accept a new socket with a new process
acceptSocket routeTree socketFd db = do
    (fd, _) <- NS.accept socketFd
    CC.forkIO $ acceptBody routeTree fd db
    acceptSocket routeTree socketFd db

acceptBody :: Route.RouteTree -> NS.Socket -> DB.Connection -> IO () 
-- ^ Process the connection
acceptBody routeTree fd db = do
    req <- NSB.recv fd 4096
    let request = Req.parse req fd
    let uri = Req.uri request
    let method = Req.method request
    let handler = Route.match uri method routeTree
    putStrLn $ show request    
    response <- (Route.match uri method routeTree) db request
    NSB.sendAll fd $ Res.render response
    NS.sClose fd

