{-# LANGUAGE OverloadedStrings #-}
module Core.JsonSpec where

import qualified Data.ByteString.Char8       as BS
import qualified Data.Map.Strict                as M
import Core.Json
import SpecHelper


spec :: Spec
spec = do
    describe "Core.JsonSpec" $ do
        context "Simple Text" $ do
            it "parses simple string" $ do
                 parse " \"Sample String\" " `shouldBe`
                     JSString "Sample String"
            it "parses simple array" $ do
                 parse " [ \"1\",\"2\"\t, \"3\" ]" `shouldBe` 
                     JSArray [JSString "1", JSString "2", JSString "3"]
            it "parses simple object" $ do
                 parse " { \"key\":[\"value\"] } " `shouldBe`
                     JSObject (M.fromList [("key", JSArray [JSString "value"])])
            it "parses quoted string" $ do
                 parse " \" \\\" Hello \" " `shouldBe`
                     JSString " \" Hello "

main :: IO ()
main = hspec spec
