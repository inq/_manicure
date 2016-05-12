{-# LANGUAGE OverloadedStrings #-}
module Core.ByteStringSpec where

import qualified Data.ByteString.Char8          as BS
import qualified Data.Map.Strict                as M
import Core.ByteString
import SpecHelper

spec :: Spec
spec = do
    describe "Core.ByteStringSpec" $ do
        context "Simple Text" $ do
            it "parses simple string" $ do
                splitAndDecode '&' " hello = hihi" `shouldBe`
                    M.fromList [("hello", "hihi")]
            it "parses three arguments" $ do
                splitAndDecode ';' " hello=hihi;a     =b; c = e" `shouldBe`
                    M.fromList [("hello", "hihi"), ("a", "b"), ("c", "e")]

main :: IO ()
main = hspec spec
