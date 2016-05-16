{-# LANGUAGE OverloadedStrings #-}
module Core.JsonSpec where

import qualified Data.Map.Strict                as M
import Core.Json
import SpecHelper

spec :: Spec
spec = 
  describe "Core.JsonSpec" $ 
    context "Simple Text" $ do
      it "parses simple string" $ 
        parse " \"Sample String\" " `shouldBe`
          JSString "Sample String"
      it "parses simple array" $ 
        parse " [ \"1\",\"2\"\t, \"3\" ]" `shouldBe` 
          JSArray [JSString "1", JSString "2", JSString "3"]
      it "parses simple object" $ 
        parse " { \"key\":[\"value\"] } " `shouldBe`
          JSObject (M.fromList [("key", JSArray [JSString "value"])])
      it "parses quoted string" $ 
        parse " \" \\\" Hello \" " `shouldBe`
          JSString " \" Hello "

main :: IO ()
main = hspec spec
