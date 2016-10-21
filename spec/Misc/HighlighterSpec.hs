{-# LANGUAGE OverloadedStrings #-}
module Misc.HighlighterSpec where

import SpecHelper

spec :: Spec
spec =
  describe "Core.HighlighterSpec" $ do
    context "Token parser" $ do
      it "parses simple string" $ do
        (1 :: Int) `shouldBe` 1

main :: IO ()
main = hspec spec
