module Misc.Highlighter.Snippet
  ( Token (..)
  , Snippet
  ) where

import qualified Data.ByteString.Char8 as BS

type Snippet = [(Token, BS.ByteString)]

data Token
  = Normal
  | Keyword
  | Function
  | Operator
