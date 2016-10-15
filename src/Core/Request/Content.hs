{-# LANGUAGE OverloadedStrings #-}
module Core.Request.Content where

import qualified Data.Attoparsec.ByteString.Lazy as AL
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as LS
import qualified Misc.Parser as P
import qualified Data.Map as M
import qualified Core.Request.ContentDisposition as Cont
import Control.Applicative (many)
import Misc.ByteString (QueryString, splitAndDecode)
import Data.Maybe (fromJust)
import Data.Char (toLower)

-- * Data types

type Content = M.Map BS.ByteString Context

data Context
  = MkText BS.ByteString
  | MkFile
    { filename :: Maybe BS.ByteString
    , contentType :: Maybe BS.ByteString
    , content :: BS.ByteString
    }
  deriving (Eq, Show)

getBoundary :: P.Parser BS.ByteString
-- ^ Parse and read the boundary
getBoundary = P.string "multipart/form-data" *> P.skipSpace
  *> P.char ';' *> P.skipSpace *> P.string "boundary="
  *> P.noneOf1 " "


parseMultipart :: BS.ByteString -> P.Parser Content
parseMultipart boundary = do
  _ <- P.string "--" <* P.string boundary <* P.string "\r\n"
  headers <- (M.fromList <$> many header) <* P.endOfLine
  (name, fname) <- case M.lookup "content-disposition" headers of
    Just disp -> case P.parseOnly Cont.parse disp of
      Right val -> return (Cont.name val, Cont.filename val)
      Left _ -> return (Nothing, Nothing)
    _ -> return (Nothing, Nothing)
  cont <- BS.pack <$> P.manyTill P.anyChar (P.try $ P.string "\r\n--")
    <* P.string boundary
  return $ M.fromList $ case name of
    Just n ->
      [( n
       , MkFile
         { filename = fname
         , contentType = M.lookup "content-type" headers
         , content = cont
         }
       )]
    Nothing -> []
 where
  header = (,)
    <$> (BS.map toLower
      <$> (P.takeWhile P.isToken <* P.char ':' <* P.skipWhile P.isHorizontalSpace))
    <*> (P.takeTill P.isEndOfLine <* P.endOfLine)

fromQS :: QueryString -> Content
fromQS = M.map MkText

mkContent :: Maybe BS.ByteString -> LS.ByteString -> Content
mkContent contType cont = case fromJust contType of
  "application/x-www-form-urlencoded"
    -> fromQS $ splitAndDecode '&' $ LS.toStrict cont
  x -> case P.parseOnly getBoundary x of
      Left _ -> M.empty
      Right b -> case P.parse (parseMultipart b) cont of
        AL.Done _ res -> res
        AL.Fail _ _ _ -> M.empty

lookup :: BS.ByteString -> Content -> Maybe Context
lookup = M.lookup
