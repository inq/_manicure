{-# LANGUAGE OverloadedStrings    #-}
module Core.Markdown where

import qualified Data.ByteString.Char8            as BS
import qualified Data.ByteString.Lazy             as LS
import qualified Core.Parser                      as P
import qualified Data.Attoparsec.ByteString.Lazy  as AL
import Control.Applicative ((<|>))

data Markdown = Markdown [Item]
          deriving (Eq, Show)
data Item = H5  !LS.ByteString 
          | H4  !LS.ByteString
          | H3  !LS.ByteString
          | H2  !LS.ByteString
          | H1  !LS.ByteString
          | Quote  !LS.ByteString
          | Paragraph  !LS.ByteString 
          deriving (Eq, Show)

parse :: LS.ByteString -> Markdown
-- ^ Parse the given bytestring
parse str = case P.parse parseMarkdown str of
    AL.Done _ val -> val
    _    -> error "markdown: parse error"

parseItem :: P.Parser Item
-- ^ The subparser
parseItem = (parseHeader <|> parseQuote <|> parseParagraph) <* P.many1 (P.char '\n')
  where
    parseHeader = do
        sharps <- P.try (P.many1 (P.char '#')) <* P.spaces
        rest <- LS.fromStrict <$> P.noneOf1 "\n"
        return $ case length sharps of
            1 -> H1 rest
            2 -> H2 rest
            3 -> H3 rest
            4 -> H4 rest
            5 -> H5 rest
            _ -> error "not implemented"
    parseQuote = do
        _ <- P.try (P.char '>') <* P.spaces
        rest <- LS.fromStrict <$> P.noneOf1 "\n"
        return $ Quote rest
    parseParagraph = do
        rest <- LS.fromStrict <$> P.noneOf1 "\n"
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
toStr (Quote str) = LS.concat ["<blockquote><p>", str, "</p></blockquote>"]
toStr (Paragraph str) = LS.concat ["<p>", str, "</p>"]
