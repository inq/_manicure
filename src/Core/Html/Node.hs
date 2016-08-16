{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
module Core.Html.Node
  ( Node (..)
  , Attr (..)
  , Token (..)
  , parseLine
  ) where

import qualified Data.ByteString.UTF8             as UTF8
import qualified Core.Parser                      as P
import Control.Applicative ((<|>))

-- * Data types
data Token
  = TStr !String
  | TRef !String
  deriving Show

data Node
  = NTag !String ![Attr] ![Node]
  | NBts ![Token]
  | NStr ![Token]
  | NMon ![Token]
  | NMap !String !String ![Node]
  | NIf ![String] ![Node]
  deriving Show

data Attr
  = Attr !String !Token
  deriving Show

-- * Parser

parseToken :: P.Parser Token
-- ^ Parse the token.
parseToken = do
    c <- P.spaces *> P.peekChar'
    res <- case c of
      '\'' -> TStr . UTF8.toString <$> (P.anyChar *> P.noneOf1 "\'" <* P.char '\'')
      '\"' -> TStr . UTF8.toString <$> (P.anyChar *> P.noneOf1 "\"" <* P.char '\"')
      '\n' -> fail "newline reached"
      _ -> TRef . UTF8.toString <$> (P.noneOf1 ",}\n ")
    P.spaces
    return res

parseTag :: P.Parser Node
-- ^ Parse the tag.
parseTag = NTag
    <$> UTF8.toString <$> (P.noneOf " \n")
    <*> (P.try parseArgs <|> return [])
    <*> return []
  where
    parseArgs = P.token '{' *> (P.sepBy parseArg $ P.char ',') <* P.char '}'
    parseArg = Attr
        <$> (UTF8.toString <$> (P.skipSpace *> P.noneOf " :"))
        <*> (P.skipSpace *> P.char ':' *> P.skipSpace *> parseToken)

parseCommand :: P.Parser Node
-- ^ Parse commands: render, if, map.
parseCommand = do
    c <- P.anyChar *> P.skipSpace *> P.peekChar'
    case c of
        'i' -> ifNode
        'm' -> mapNode
        c' -> error $ "unexpected char(" ++ [c'] ++ ")"
  where
    ifNode = P.string "if" *> P.skipSpace *> (
        NIf
        <$> (map UTF8.toString <$> (P.sepBy (P.spaces *> P.noneOf " \n") $ P.char ' '))
        <*> return []
      )
    mapNode = P.string "map" *> P.skipSpace *> (
        NMap
        <$> UTF8.toString <$> P.noneOf " "
        <*> (UTF8.toString <$>
              (P.spaces *> P.string "->" *> P.spaces *> P.noneOf " \n" <* P.spaces))
        <*> return []
      )

parseLine :: P.Parser (Int, Node)
-- ^ Parsing a line, get indent level & node information.
parseLine = do
    i <- indents
    c <- P.peekChar'
    tag <- case c of
        '|' -> textNode
        '$' -> strNode
        '=' -> btsNode
        '^' -> monadNode
        '-' -> parseCommand
        _ -> parseTag
    _ <- P.char '\n'
    return (i, tag)
  where
    indents = sum <$> P.many (
        (P.char ' ' >> return 1) <|>
        (P.char '\t' >> fail "tab charactor is not allowed")
      )
    strNode = P.anyChar *> P.skipSpace *>
        (NStr <$> P.many1 parseToken)
    btsNode = P.anyChar *> P.skipSpace *>
        (NBts <$> P.many1 parseToken)
    monadNode = P.anyChar *> P.skipSpace *>
        (NMon <$> P.many1 parseToken)
    textNode = P.anyChar *> P.skipSpace *>
        (NStr . (:[]) . TStr . UTF8.toString <$> P.noneOf "\n")
