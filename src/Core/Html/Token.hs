{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}
module Core.Html.Token
  ( Token(..)
  ) where

import qualified Language.Haskell.TH.Syntax       as TS
import qualified Data.ByteString.UTF8             as UTF8
import qualified Core.ByteString                  as ByteString

-- * Data types
data Token
  = TStr !String
  | TVal !String
  deriving Show

instance TS.Lift Token where
    lift (TStr a) = [| UTF8.fromString a |]
    lift (TVal a) = [| ByteString.convert $(return $ TS.VarE $ TS.mkName a) |]
