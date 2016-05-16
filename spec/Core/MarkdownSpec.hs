{-# LANGUAGE OverloadedStrings #-}
module Core.MarkdownSpec where

import Core.Markdown
import SpecHelper


spec :: Spec
spec = 
  describe "Core.MarkdownSpec" $ 
    context "Simple Text" $ 
      it "parses simple string" $ 
        parse "#### Hello\n\n" `shouldBe`
          Markdown [H4 "Hello"]

main :: IO ()
main = hspec spec
