{-# LANGUAGE OverloadedStrings #-}
module Misc.MarkdownSpec where

import Misc.Markdown
import SpecHelper

spec :: Spec
spec =
  describe "Core.MarkdownSpec" $ do
    context "Simple Text" $ do
      it "parses simple string" $ do
        parse "#### Hello\r\n\r\n" `shouldBe`
          Just (Markdown [H4 "Hello"])
        parse "Hello\r\n\r\nHihi\r\n" `shouldBe`
          Just (Markdown [Paragraph "Hello", Paragraph "Hihi"])
      it "parses simple enippet" $
        parse "Hello\r\n```rust\r\nHELLO\r\n```\r\n" `shouldBe`
          Just (Markdown [Paragraph "Hello", Snippet "rust" ["HELLO"]])
    context "To Html" $ do
      it "generate h4 string" $
        toHtml (Markdown [Paragraph "HI"]) `shouldBe`
          "<p>HI</p>"
      it "generate paragraphs" $
        toHtml (Markdown [Paragraph "HI", Paragraph "Hello"]) `shouldBe`
          "<p>HI</p><p>Hello</p>"

main :: IO ()
main = hspec spec
