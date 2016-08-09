{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE EmptyCase         #-}
{-# LANGUAGE FlexibleContexts  #-}
module Core.Html
  ( parse
  ) where

import qualified Language.Haskell.TH.Quote        as TQ
import qualified Data.ByteString.UTF8             as UTF8
import qualified Core.Parser                      as P
import Core.Html.Node (Node(..), parseLine)
import Core.Html.Meta (MetaNode(..), convert)

-- * TH

parseNode :: P.Parser [MetaNode]
-- ^ The main parser
parseNode = do
    (_, res, _) <- buildTree <$> P.many parseLine
    return $ concatMap convert res

parse :: TQ.QuasiQuoter
-- ^ Parser for QuasiQUoter
parse = TQ.QuasiQuoter {
        TQ.quoteExp = quoteExp,
        TQ.quotePat = undefined,
        TQ.quoteType = undefined,
        TQ.quoteDec = undefined
    }
  where
    quoteExp str = do
        case P.parseOnly parseNode (UTF8.fromString str) of
            Right tag -> [| return tag |]
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
    replace (NForeach vals val _) = NForeach vals val
    replace (NTag name attr _) = NTag name attr
    replace (NIf args _) = NIf args
    replace (NText _) = error "indentation error"
buildTree []  =
    (0, [], [])
