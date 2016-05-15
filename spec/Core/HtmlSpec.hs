{-# LANGUAGE OverloadedStrings #-}
{-# LANUGAGE Templatehaskell #-}
{-# LANGUAGE QuasiQuotes #-}
module Core.HtmlSpec where

import qualified Data.ByteString.Char8            as BS
import Core.Html
import SpecHelper



spec :: Spec
spec = 
    describe "Core.HtmlSpec" $ 
        context "Simple Text" $ do
            it "parses simple tag" $ 
                [parse|html
                  div
                    | Hello
                  |] `shouldBe` "<html><div>Hello</div></html>"
            it "parses simple variable" $ 
                [parse|html
                  div
                    = theValue
                  |] `shouldBe` "<html><div>VALUE</div></html>"
            it "parses simple tag" $ 
                [parse|html
                  div
                    | Hello
                  |] `shouldBe` "<html><div>Hello</div></html>"
            it "processes simple foreach statement" $
                [parse|html
                  - foreach lines -> name, title
                    div
                      p
                        = name
                      p
                        = title
                  |] `shouldBe` "<html><div><p>A</p><p>B</p></div></html>"
          where
              theValue = "VALUE" :: BS.ByteString
              lines = [["A", "B"] :: [BS.ByteString]]


main :: IO ()
main = hspec spec
