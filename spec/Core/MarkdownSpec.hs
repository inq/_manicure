{-# LANGUAGE OverloadedStrings #-}
module Core.MarkdownSpec where

import Core.Markdown
import SpecHelper

spec :: Spec
spec = 
  describe "Core.MarkdownSpec" $ do
    context "Simple Text" $ do
      it "parses simple string" $ 
        parse "#### Hello\n\n" `shouldBe`
          Just (Markdown [H4 "Hello"])
      it "parses simple enippet" $
        parse "Hello\n```rust\nHELLO\n```\n" `shouldBe`
          Just (Markdown [Paragraph "Hello", Snippet "rust" ["HELLO"]])
    context "To Html" $
      it "generate h4 string" $ 
        toHtml (Markdown [Paragraph "HI"]) `shouldBe`
          "<p>HI</p>"

main :: IO ()
main = hspec spec
