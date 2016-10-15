{-# LANGUAGE OverloadedStrings #-}
module Core.Request.Content where

import qualified Data.Attoparsec.ByteString.Lazy as AL
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as LS
import qualified Misc.Parser as P
import qualified Data.Map as M
import Control.Applicative (many)
import Misc.ByteString (QueryString, splitAndDecode)
import Data.Maybe (fromJust)
import Data.Char (toLower)
import Control.Applicative ((<|>))

-- * Data types

type Content = M.Map BS.ByteString Context

data Context
  = MkText BS.ByteString
  | MkFile
    { filename :: Maybe BS.ByteString
    , contentType :: Maybe BS.ByteString
    , content :: BS.ByteString
    }
  deriving Show

getBoundary :: P.Parser BS.ByteString
-- ^ Parse and read the boundary
getBoundary = P.string "multipart/form-data" *> P.skipSpace
  *> P.char ';' *> P.skipSpace *> P.string "boundary="
  *> P.noneOf1 " "


data ContentDisposition
  = MkContDisp
    { cdTag :: ContDispType
    , cdName :: Maybe BS.ByteString
    , cdFilename :: Maybe BS.ByteString
    }
  deriving (Eq, Show)

data ContDispType = Inline | Attachment | FormData
  deriving (Eq, Show)

parseContDisp :: P.Parser ContentDisposition
-- ^ https://www.w3.org/Protocols/rfc2616/rfc2616-sec19.html
parseContDisp = do
  bstag <- BS.map toLower <$> P.takeTill' (== ';') <* P.char ';'
  tag <- case bstag of
    "form-data" -> return FormData
    "inline" -> return Inline
    "attachment" -> return Attachment
    _ -> fail "Invalid ContentDisposition"
  rmap <- M.fromList
    <$> P.sepBy
      ( (,)
      <$> (P.spaces *> P.noneOf "= " <* P.spaces <* P.char '=')
      <*> (P.spaces *> P.quoted)
      ) (P.char ';')
  return $ MkContDisp tag (M.lookup "name" rmap) (M.lookup "filename" rmap)


parseMultipart :: BS.ByteString -> P.Parser Content
parseMultipart boundary = do
  P.string "--" <* P.string boundary <* P.string "\r\n"
  headers <- M.fromList <$> (many header <* P.endOfLine)
  (name, fname, ctype) <- case M.lookup "content-disposition" headers of
    Just disp -> do
      let (n, f) = case P.parseOnly parseContDisp disp of
           Right val -> (cdName val, cdFilename val)
           Left _ -> (Nothing, Nothing)
      return (Just disp, f, Just "")
    _ -> return (Nothing, Nothing, Nothing)
  cont <- BS.pack <$> P.manyTill P.anyChar (P.try $ P.string "\r\n--")
    <* P.string boundary
  return $ M.fromList $ case name of
    Just n ->
      [( n
       , MkFile
         { filename = fname
         , contentType = ctype
         , content = cont
         }
       )]
    Nothing -> []
 where
  header = (,)
    <$> (BS.map toLower <$> (P.takeWhile P.isToken <* P.char ':' <* P.skipWhile P.isHorizontalSpace))
    <*> (P.takeTill P.isEndOfLine <* P.endOfLine)

fromQS :: QueryString -> Content
fromQS qs = M.map (\x -> MkText x) qs

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
lookup key cont = M.lookup key cont
