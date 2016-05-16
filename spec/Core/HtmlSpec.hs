{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Core.HtmlSpec where

import qualified Data.ByteString.Char8            as BS
import Core.Html
import SpecHelper

spec :: Spec
spec = 
  describe "Core.HtmlSpec" $ do
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
          - foreach people -> name, title
            div
              p
                = name
              p
                = title
          |] `shouldBe` "<html><div><p>A</p><p>B</p></div></html>"
    context "External File" $ do
      it "parses simple partial" $
        [parse|html
          p
            - render simple.qh
          |] `shouldBe` "<html><p><div>Hello</div></p></html>"
      it "parses simple variable" $
        [parse|html
          ul
            - foreach people -> name, title
              - render variable.qh
          |] `shouldBe` "<html><ul><li><span>A</span><pan>B</pan></li></ul></html>"
    context "If statement" $ do      
      it "parses true statement" $
        [parse|html
          div
            - if trueStatement
              p
                | Hello
          |] `shouldBe` "<html><div><p>Hello</p></div></html>"
      it "parses false statement" $
        [parse|html
          div
            - if falseStatement
              p
                | Hello
          |] `shouldBe` "<html><div></div></html>"
      it "applies true function" $
        [parse|html
          div
            - if graterThan four three
              p
                | Hello
          |] `shouldBe` "<html><div><p>Hello</p></div></html>"
      it "applies false function" $
        [parse|html
          div
            - if graterThan three four
              p
                | Hello
          |] `shouldBe` "<html><div></div></html>"
 where
  theValue = "VALUE" :: BS.ByteString
  people = [["A", "B"] :: [BS.ByteString]]
  trueStatement = True
  falseStatement = False
  graterThan = (>)
  three = 3 :: Integer 
  four = 4 :: Integer
        


main :: IO ()
main = hspec spec
