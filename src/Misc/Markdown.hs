{-# LANGUAGE OverloadedStrings #-}
module Misc.Markdown where

import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as LS
import qualified Misc.Parser as P
import Data.Attoparsec.ByteString.Lazy (Result(Done))
import Control.Applicative ((<|>))

-- * Data types

data Markdown
  = Markdown [Item]
  deriving (Eq, Show)

data Item
  = H5  !LS.ByteString
  | H4  !LS.ByteString
  | H3  !LS.ByteString
  | H2  !LS.ByteString
  | H1  !LS.ByteString
  | Img !LS.ByteString !LS.ByteString
  | Quote  !LS.ByteString
  | Paragraph  !LS.ByteString
  | Snippet !LS.ByteString ![LS.ByteString]
  deriving (Eq, Show)

-- * Parsers

parse :: LS.ByteString -> Maybe Markdown
-- ^ Parse the given bytestring
parse str = case P.parse parseMarkdown str of
    Done _ val -> Just val
    _ -> Nothing

parseItem :: P.Parser Item
-- ^ The subparser
parseItem = (parseHeader <|> parseSnippet <|> parseQuote <|> parseImage <|> parseParagraph)
      <* P.many1 (P.string "\r\n")
  where
    parseEnd = do
        _ <- P.try (P.string "```")
        return []
    parseLine = parseEnd <|>
        (((:) . LS.fromStrict) <$>
         (P.noneOf "\r" <* P.string "\r\n") <*>
         parseLine)
    parseSnippet = do
        open <- LS.fromStrict <$>
            P.try (P.string "```" *> P.noneOf1 "\r" <* P.string "\r\n")
        res <-  parseLine
        return $ Snippet open res
    parseImage = do
        alt <- LS.fromStrict <$>
            P.try (P.char '!' *> P.spaces *> P.noneOf1 ";" <* P.char ';' <* P.spaces)
        uri <- LS.fromStrict <$> P.noneOf1 "\r" <* P.string "\r\n"
        return $ Img alt uri
    parseHeader = do
        sharps <- P.try (P.many1 (P.char '#')) <* P.spaces
        rest <- LS.fromStrict <$> P.noneOf1 "\r\n"
        return $ case length sharps of
            1 -> H1 rest
            2 -> H2 rest
            3 -> H3 rest
            4 -> H4 rest
            5 -> H5 rest
            _ -> error "not implemented"
    parseQuote = do
        _ <- P.try (P.char '>') <* P.spaces
        rest <- LS.fromStrict <$> P.noneOf1 "\r\n"
        return $ Quote rest
    parseParagraph = do
        rest <- LS.fromStrict <$> P.noneOf1 "\r\n"
        return $ Paragraph rest

parseMarkdown :: P.Parser Markdown
-- ^ The actual parser
parseMarkdown = do
    items <- P.many1 parseItem
    return $ Markdown items

toHtml :: Markdown -> LS.ByteString
-- ^ Generate html
toHtml (Markdown items) = LS.concat $ map toStr items

toStr :: Item -> LS.ByteString
-- ^ Convert item to string
toStr (H5 str) = LS.concat ["<h5>", str, "</h5>"]
toStr (H4 str) = LS.concat ["<h4>", str, "</h4>"]
toStr (H3 str) = LS.concat ["<h3>", str, "</h3>"]
toStr (H2 str) = LS.concat ["<h2>", str, "</h2>"]
toStr (H1 str) = LS.concat ["<h1>", str, "</h1>"]
toStr (Img alt uri) = LS.concat
        [ "<img class='content-img' alt='"
        , alt
        , "' src='"
        , uri
        , "'>"]
toStr (Quote str) = LS.concat ["<blockquote><p>", str, "</p></blockquote>"]
toStr (Paragraph str) = LS.concat ["<p>", str, "</p>"]
toStr (Snippet _ strs) = LS.concat
      ("<table class='code-snippet'>" :
       contents (1 :: Integer) strs ++ ["</table>"])
  where
    contents line (str : strs') = LS.concat
        [ "<tr><td class='td-line-num' data-line-num='"
        , LS.fromStrict $ BS.pack $ show line
        , "'/><td class='td-content'>"
        , str, "</td></tr>"
        ] : contents (line + 1) strs'
    contents _ [] = []

convert :: LS.ByteString -> Maybe LS.ByteString
-- ^ Convert markdown to html
convert markdown = toHtml <$> parse markdown
