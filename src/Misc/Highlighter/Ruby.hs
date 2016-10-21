{-# LANGUAGE OverloadedStrings #-}
module Misc.Highlighter.Ruby
  ( parseRuby
  ) where

import qualified Data.ByteString.Char8 as BS
import qualified Misc.Parser as P
import Misc.Highlighter.Snippet ( Token(..), Snippet )

parseRuby :: P.Parser Snippet
parseRuby = return [(Keyword, "HELLO")]
