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
  | TVal !String
  | TMon !String
  deriving Show

data Node
  = NTag !String ![Attr] ![Node]
  | NText !Token
  | NForeach !String ![String] ![Node]
  | NIf ![String] ![Node]
  deriving Show

data Attr
  = Attr !String !Token
  deriving Show

-- * Parser

parseToken :: P.Parser Token
-- ^ Parse the token.
parseToken = do
    P.skipSpace *> P.char ':' *> P.skipSpace
    c <- P.skipSpace *> P.peekChar'
    res <- case c of
      '\'' -> TStr . UTF8.toString <$> (P.anyChar *> P.noneOf1 "\'" <* P.char '\'')
      '\"' -> TStr . UTF8.toString <$> (P.anyChar *> P.noneOf1 "\"" <* P.char '\"')
      _ -> (TVal . UTF8.toString <$> (P.noneOf1 ",} "))
    P.skipSpace
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
        <*> parseToken

parseCommand :: P.Parser Node
-- ^ Parse commands: render, if, foreach.
parseCommand = do
    c <- P.anyChar *> P.skipSpace *> P.peekChar'
    case c of
        'i' -> ifNode
        'f' -> foreachNode
        c' -> error $ "unexpected char(" ++ [c'] ++ ")"
  where
    ifNode = P.string "if" *> P.skipSpace *> (
        NIf
        <$> (map UTF8.toString <$> (P.sepBy (P.spaces *> P.noneOf " \n") $ P.char ' '))
        <*> return []
      )
    foreachNode = P.string "foreach" *> P.skipSpace *> (
        NForeach
        <$> UTF8.toString <$> P.noneOf " "
        <*> (map UTF8.toString <$>
              (P.string " -> " *>
                (P.sepBy (P.spaces *> P.noneOf " ,\n") $ P.char ',')))
        <*> return []
      )

parseLine :: P.Parser (Int, Node)
-- ^ Parsing a line, get indent level & node information.
parseLine = do
    i <- indents
    c <- P.peekChar'
    tag <- case c of
        '|' -> textNode
        '=' -> valueNode
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
    valueNode = do
        P.anyChar *> P.skipSpace
        val <- P.noneOf "\n"
        return $ (NText . TVal) $ UTF8.toString val
    monadNode = do
        P.anyChar *> P.skipSpace
        val <- P.noneOf "\n"
        return $ (NText . TMon) $ UTF8.toString val
    textNode = P.anyChar *> P.skipSpace *> ((NText . TStr) <$> UTF8.toString <$> P.noneOf "\n")
