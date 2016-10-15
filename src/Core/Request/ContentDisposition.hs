{-# LANGUAGE OverloadedStrings #-}
module Core.Request.ContentDisposition where

import qualified Data.ByteString.Char8 as BS
import qualified Misc.Parser as P
import qualified Data.Map as M
import Data.Char (toLower)

-- * Data types

data ContentDisposition
  = ContentDisposition
    { dispType :: Type
    , name :: Maybe BS.ByteString
    , filename :: Maybe BS.ByteString
    }
  deriving (Eq, Show)

data Type = Inline | Attachment | FormData
  deriving (Eq, Show)

-- * Parser

parse :: P.Parser ContentDisposition
-- ^ https://www.w3.org/Protocols/rfc2616/rfc2616-sec19.html
parse = do
  dtypebs <- BS.map toLower <$> P.takeTill' (== ';') <* P.char ';'
  dtype <- case dtypebs of
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
  return $ ContentDisposition dtype (M.lookup "name" rmap) (M.lookup "filename" rmap)
