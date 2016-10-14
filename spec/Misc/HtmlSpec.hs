{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
module Misc.HtmlSpec where

import qualified Data.ByteString.Char8            as BS
import qualified Data.ByteString.UTF8             as UTF8
import Misc.Html
import SpecHelper

data Person = Person
  { nP :: String
  , mP :: String
  , oP :: String
  }

spec :: Spec
spec =
  describe "Core.HtmlSpec" $ do
    context "Token parser" $ do
      it "parses simple string" $ do
        res <- [parse|html
          div { class= 'hello', id= "hihi" }
            | hi
         |]
        res `shouldBe` UTF8.fromString
          "<html><div class=\"hello\" id=\"hihi\">hi</div></html>"
    context "UTF-8 Text" $ do
      it "parses simple utf-8" $ do
        res <- [parse|html
          div
            | 안녕
         |]
        res `shouldBe` UTF8.fromString "<html><div>안녕</div></html>"
    context "Monadic Context" $ do
      it "parses monad combination" $ do
        let inner = [parse|p
           | inner
         |] :: IO BS.ByteString
        res <- [parse|html
          div
            ^ inner
            | outer
         |]
        res `shouldBe` UTF8.fromString
          "<html><div><p>inner</p>outer</div></html>"
      it "parses monad combinating function" $ do
        let inner v = [parse|p
           | inner
           = v
         |]
        let arg = "center" :: BS.ByteString
        res <- [parse|html
          div
            ^ inner arg
            | outer
         |]
        res `shouldBe` UTF8.fromString
          "<html><div><p>innercenter</p>outer</div></html>"
    context "Simple Text" $ do
      it "parses simple tag" $ do
        res <- [parse|html
          div
            | Hello
         |]
        res `shouldBe` "<html><div>Hello</div></html>"
      it "parses simple variable" $ do
        let  theValue = "VALUE" :: BS.ByteString
        res <- [parse|html
          div
            = theValue
         |]
        res `shouldBe` "<html><div>VALUE</div></html>"
      it "parses simple function" $ do
        let theFunc x = BS.concat ["---", x, "---"]
        let theVal = "HELLO"
        res <- [parse|html
          div
            = theFunc theVal
         |]
        res `shouldBe` "<html><div>---HELLO---</div></html>"
      it "parses simple function with string" $ do
        let theFunc x = BS.concat ["---", x, "---"]
        res <- [parse|html
          div
            = theFunc "HIHI"
         |]
        res `shouldBe` "<html><div>---HIHI---</div></html>"
      it "parses simple tag" $ do
        res <- [parse|html
          div
            | Hello
         |]
        res `shouldBe` "<html><div>Hello</div></html>"
      it "processes simple map statement" $ do
        let people = ["A", "B"] :: [BS.ByteString]
        res <- [parse|div
          - map people -> name
            p
              = name
         |]
        res `shouldBe` "<div><p>A</p><p>B</p></div>"
      it "processes complex map statement" $ do
        let people = [Person "A" "Bb" "C", Person "D" "Ee" "F"]
        res <- [parse|div
          - map people -> Person aE bE cE
            p
              $ aE
              span { class $ reverse bE }
                $ cE
         |]
        res `shouldBe`
          "<div><p>A<span class=\"bB\">C</span></p><p>D<span class=\"eE\">F</span></p></div>"
    context "If statement" $ do
      it "parses true statement" $ do
        res <- [parse|html
          div
            - if trueStatement
              p
                | Hello
         |]
        res `shouldBe` "<html><div><p>Hello</p></div></html>"
      it "parses false statement" $ do
        res <- [parse|html
          div
            - if falseStatement
              p
                | Hello
         |]
        res `shouldBe` "<html><div></div></html>"
      it "applies true function" $ do
        res <- [parse|html
          div
            - if greaterThan four three
              p
                | Hello
         |]
        res `shouldBe` "<html><div><p>Hello</p></div></html>"
      it "applies false function" $ do
        res <- [parse|html
          div
            - if greaterThan three four
              p
                | Hello
          |]
        res `shouldBe` "<html><div></div></html>"
 where
  trueStatement = True
  falseStatement = False
  greaterThan = (>) :: Integer -> Integer -> Bool
  three = 3 :: Integer
  four = 4 :: Integer

main :: IO ()
main = hspec spec
