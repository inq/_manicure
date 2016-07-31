{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
module Core.Html.Node
  ( Node (..)
  , parseLine
  ) where

import qualified Language.Haskell.TH.Syntax       as TS
import qualified Data.ByteString.Char8            as BS
import qualified Data.ByteString.UTF8             as UTF8
import qualified Core.Parser                      as P
import Core.Html.Token (Token(..))
import Control.Applicative ((<|>))

-- * Data types
data Node
  = Tag !String ![Attr] ![Node]
  | Text !Token
  | Foreach !String ![String] ![Node]
  | Render !String
  | If ![String] ![Node]
  deriving Show

data Attr
  = Dash !String ![Attr]
  | Attr !String !Token
  deriving Show

-- * Instances
instance TS.Lift Attr where
    lift (Attr name value) =
     [| BS.concat [ " ", name, "=", $(TS.lift value) ]|]
    lift _ = error "procAttrs: Dash is not allowed"

-- * Parser

parseTag :: P.Parser Node
-- ^ Parse the tag.
parseTag = Tag
    <$> UTF8.toString <$> (P.noneOf " \n")
    <*> (P.try parseArgs <|> return [])
    <*> return []
  where
    parseArgs = P.token '{' *> (P.sepBy parseArg $ P.token ',') <* P.char '}'
    parseArg = Attr
        <$> (UTF8.toString <$> (P.noneOf " :"))
        <*> (TStr . UTF8.toString <$> (P.token ':' *> P.noneOf1 ",}"))

parseCommand :: P.Parser Node
-- ^ Parse commands: render, if, foreach.
parseCommand = do
    c <- P.anyChar *> P.skipSpace *> P.peekChar'
    case c of
        'r' -> renderNode
        'i' -> ifNode
        'f' -> foreachNode
        c' -> error $ "unexpected char(" ++ [c'] ++ ")"
  where
    renderNode = P.string "render" *> P.skipSpace *> (Render <$> UTF8.toString <$> P.noneOf "\n")
    ifNode = P.string "if" *> P.skipSpace *> (
        If
        <$> (map UTF8.toString <$> (P.sepBy (P.spaces *> P.noneOf " \n") $ P.char ' '))
        <*> return []
      )
    foreachNode = P.string "foreach" *> P.skipSpace *> (
        Foreach
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
        return $ (Text . TVal) $ UTF8.toString val
    textNode = P.anyChar *> P.skipSpace *> ((Text . TStr) <$> UTF8.toString <$> P.noneOf "\n")
