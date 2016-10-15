{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE EmptyCase, FlexibleContexts  #-}
module Misc.Html
  ( parse
  ) where

import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.UTF8 as UTF8
import Language.Haskell.TH.Quote (QuasiQuoter(..))
import Misc.Html.Node (Node(..), parseLine)
import Misc.Html.Meta (MetaNode(..), optimize, convert)
import Misc.Parser (Parser, many, parseOnly)

-- * TH

parseNode :: Parser [MetaNode]
-- ^ The main parser
parseNode = do
    (_, res, _) <- buildTree <$> many parseLine
    return $ optimize $ concatMap convert res

parse :: QuasiQuoter
-- ^ Parser for QuasiQUoter
parse = QuasiQuoter
  { quoteExp = qExp
  , quotePat = undefined
  , quoteType = undefined
  , quoteDec = undefined
  }
 where
  qExp str =
    case parseOnly parseNode (UTF8.fromString str) of
      Right tag -> [| BS.concat <$> sequence tag |]
      Left _    -> undefined

-- * Node

buildTree :: [(Int, Node)] -> (Int, [Node], [(Int, Node)])
-- ^ Using the indent size and node information, build the Node tree.
buildTree ((indent, node) : rest)
    | indent < next = buildTree $ (indent, replace node res) : remaining
    | indent > next = (indent, [node], rest)
    | otherwise  = (indent, (node) : res, remaining)
  where
    (next, res, remaining) = buildTree rest
    replace (NMap vals val _) = NMap vals val
    replace (NTag name attr _) = NTag name attr
    replace (NIf args _) = NIf args
    replace _ = error "indentation error"
buildTree []  =
    (0, [], [])
