{-# LANGUAGE OverloadedStrings    #-}
module Core.Markdown where

import qualified Data.ByteString.Char8            as BS
import qualified Core.Parser                      as P

import Control.Applicative ((*>), (<*), (<$>), (<*>), (<|>))

data Markdown = Markdown [Item]
          deriving (Eq, Show)
data Item = H5 BS.ByteString | H4 BS.ByteString
          | H3 BS.ByteString | H2 BS.ByteString
          | H1 BS.ByteString
          | Quote BS.ByteString
          | Paragraph BS.ByteString 
          deriving (Eq, Show)

parse :: BS.ByteString -> Markdown
-- ^ Parse the given bytestring
parse str = case P.parseOnly parseMarkdown str of
    Right val -> val
    Left err  -> undefined

parseItem :: P.Parser Item
-- ^ The subparser
parseItem = (parseHeader <|> parseQuote <|> parseParagraph) <* P.many1 (P.char '\n')
  where
    parseHeader = do
        sharps <- P.try (P.many1 (P.char '#')) <* P.spaces
        rest <- P.noneOf1 "\n"
        return $ case length sharps of
            1 -> H1 rest
            2 -> H2 rest
            3 -> H3 rest
            4 -> H4 rest
            5 -> H5 rest
    parseQuote = do
        P.try (P.char '>') <* P.spaces
        rest <- P.noneOf1 "\n"
        return $ Quote rest
    parseParagraph = do
        rest <- P.noneOf1 "\n"
        return $ Paragraph rest

parseMarkdown :: P.Parser Markdown
-- ^ The actual parser
parseMarkdown = do
    items <- P.many1 parseItem
    return $ Markdown items

toHtml :: Markdown -> BS.ByteString
-- ^ Generate html
toHtml md = ""
