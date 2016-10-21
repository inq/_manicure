{-# LANGUAGE OverloadedStrings #-}
module Misc.Highlighter where

import qualified Data.ByteString.Char8 as BS
import qualified Misc.Parser as P
import Misc.Highlighter.Snippet ( Token(..), Snippet )
import Misc.Highlighter.Ruby ( parseRuby )

parsePlain :: P.Parser Snippet
parsePlain = return []

parse :: BS.ByteString -> P.Parser Snippet
parse lang =
  case lang of
    "ruby" -> parseRuby
    _ -> parsePlain
