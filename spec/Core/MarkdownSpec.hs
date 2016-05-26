{-# LANGUAGE OverloadedStrings #-}
module Core.MarkdownSpec where

import Core.Markdown
import SpecHelper


spec :: Spec
spec = 
  describe "Core.MarkdownSpec" $ do
    context "Simple Text" $ 
      it "parses simple string" $ 
        parse "#### Hello\n\n" `shouldBe`
          Markdown [H4 "Hello"]
    context "To Html" $
      it "generate h4 string" $ 
        toHtml (Markdown [Paragraph "HI"]) `shouldBe`
          "<p>HI</p>"

main :: IO ()
main = hspec spec
