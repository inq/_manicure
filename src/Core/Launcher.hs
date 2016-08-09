{-# LANGUAGE OverloadedStrings #-}
module Core.Launcher where

import qualified Network                        as N
import qualified Network.Socket                 as NS
import qualified Network.Socket.ByteString      as NSB
import qualified Network.Socket.ByteString.Lazy as NSL
import qualified Data.ByteString.Char8          as BS
import qualified Core.Database                  as DB
import qualified Control.Concurrent             as CC
import qualified System.Posix.Process           as P
import qualified System.Posix.Files             as PF
import qualified System.Posix.IO                as PIO
import qualified System.Posix.Signals           as PS
import qualified System.Directory               as D
import qualified Core.Route                     as Route
import qualified Core.Request                   as Req
import qualified Core.Response                  as Res
import qualified Core.Component                 as Com
import qualified Control.Monad                  as M

daemonize :: String -> String -> String -> IO () -> IO ()
-- ^ Daemonize the given function
daemonize pidFile stdOut stdErr process = do
    exists <- D.doesFileExist pidFile
    M.when exists $ removeAndKill pidFile
    M.void $ P.forkProcess $ do
        _ <- P.createSession
        M.void $ P.forkProcess $ do
            _ <- writePid pidFile
            remapFds
            process
  where
    remapFds = do
        devNull <- PIO.openFd "/dev/null" PIO.ReadOnly Nothing PIO.defaultFileFlags
        _ <- PIO.dupTo devNull PIO.stdInput
        PIO.closeFd devNull
        fdOut <- PIO.openFd stdOut PIO.ReadWrite (Just PF.stdFileMode) PIO.defaultFileFlags
        _ <- PIO.dupTo fdOut PIO.stdOutput
        PIO.closeFd fdOut
        fdErr <- PIO.openFd stdErr PIO.ReadWrite (Just PF.stdFileMode) PIO.defaultFileFlags
        _ <- PIO.dupTo fdErr PIO.stdError
        PIO.closeFd fdErr
    writePid _pidFile = do
        fd <- PIO.createFile _pidFile PF.stdFileMode
        pid <- P.getProcessID
        print pid
        PIO.fdWrite fd (show pid)
    removeAndKill _pidFile = do
        pid <- readFile _pidFile
        D.removeFile _pidFile
        putStrLn ("pid file exists: " ++ pid)
        PS.signalProcess PS.sigQUIT $ read pid

run :: Route.RouteTree -> BS.ByteString -> BS.ByteString -> String -> IO ()
-- ^ Run the given RouteTree
run routeTree response404 databaseName socketFile = N.withSocketsDo $ do
    removeExistingSocket socketFile
    db <- DB.connect databaseName
    socketFd <- NS.socket NS.AF_UNIX NS.Stream 0
    NS.bind socketFd $ NS.SockAddrUnix socketFile
    NS.listen socketFd 10
    PF.setFileMode socketFile PF.stdFileMode
    acceptSocket routeTree response404 socketFd db
  where
    removeExistingSocket _socketFile = do
      exists <- D.doesFileExist _socketFile
      M.when exists $ D.removeFile _socketFile

acceptSocket :: Route.RouteTree -> BS.ByteString -> NS.Socket -> DB.Connection -> IO ()
-- ^ Accept a new socket with a new process
acceptSocket routeTree response404 socketFd db = do
    (fd, _) <- NS.accept socketFd
    _ <- CC.forkIO $ acceptBody routeTree response404 fd db
    acceptSocket routeTree response404 socketFd db

acceptBody :: Route.RouteTree -> BS.ByteString -> NS.Socket -> DB.Connection -> IO ()
-- ^ Process the connection
acceptBody routeTree response404 fd db = do
    req' <- NSL.getContents fd
    let request = Req.parse req' fd
    let uri = Req.uri request
    let method = Req.method request
    (response, state) <- case Route.match uri method routeTree of
        Just (handler, params) -> do
            putStrLn "handler"
            Com.runHandler handler params db request
        Nothing -> do
            putStrLn "nothing"
            return $ (Res.error 404 response404, Com.ResState db [] request)
    print response
    NSB.sendAll fd $ Res.render response
    NS.sClose fd
