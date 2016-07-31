{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE EmptyCase         #-}
{-# LANGUAGE FlexibleContexts  #-}
module Core.Html
  ( parse
  , parseFile
  ) where

import qualified Data.ByteString.Char8            as BS
import qualified Language.Haskell.TH.Quote        as TQ
import qualified Language.Haskell.TH.Syntax       as TS
import qualified Data.ByteString.UTF8             as UTF8
import qualified Core.Parser                      as P
import Core.Html.Node (Node(..), parseLine)

-- * Data types
data Html
  = Html ![Node]
data Status
  = Child | Sibling | Parent
  deriving Show

-- * Instances
instance TS.Lift Html where
    lift (Html nodes) = [| BS.concat nodes |]

instance TS.Lift Node where
    lift (Tag string attrs nodes) =
     [| BS.concat $
          [ "<", $(TS.lift string)]
          ++ $(TS.lift attrs)
          ++ [">"]
          ++ $(TS.lift nodes)
          ++ ["</", string, ">"]
      |]
    lift (Foreach vals vs nodes) =
     [| BS.concat $ map
          (\($(return $ (TS.ListP $ map (TS.VarP . TS.mkName) vs))) -> BS.concat nodes)
          $(return $ TS.VarE $ TS.mkName vals)
     |]
    lift (Render fileName) =
     [| $(parseFile fileName) |]
    lift (If attrs nodes) =
     [| case $(return $
                (foldl (\a b -> TS.AppE a b)
                ((TS.VarE . TS.mkName . head) attrs)
                (map (TS.VarE . TS.mkName) (tail attrs)))) of
          True -> BS.concat nodes
          _ -> ""
      |]
    lift (Text a) = [| a |]

instance TS.Lift Status where
    lift Child   = [| Child |]
    lift Sibling = [| Sibling |]
    lift Parent  = [| Parent |]

-- * TH

parseFile :: FilePath -> TS.Q TS.Exp
-- ^ Parse the given file.
parseFile filePath = do
    TS.qAddDependentFile path
    s <- TS.qRunIO $ readFile path
    TQ.quoteExp parse s
  where
    path = "views/" ++ filePath

parseNode :: P.Parser Html
-- ^ The main parser
parseNode = do
    (_, res, _) <- buildTree <$> P.many parseLine
    return (Html res)

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
            Right tag -> [| tag |]
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
    replace (Foreach vals val _) = Foreach vals val
    replace (Tag name attr _) = Tag name attr
    replace (If args _) = If args
    replace (Render _) = error "indentation error"
    replace (Text _) = error "indentation error"
buildTree []  =
    (0, [], [])
