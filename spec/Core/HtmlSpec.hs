{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
module Core.HtmlSpec where

import qualified Data.ByteString.Char8            as BS
import qualified Data.ByteString.UTF8             as UTF8
import Core.Html
import SpecHelper

spec :: Spec
spec =
  describe "Core.HtmlSpec" $ do
    context "Token parser" $ do
      it "parses simple string" $ do
        res <- [parse|html
          div { class: 'hello', id: "hihi" }
            | hi
         |]
        res `shouldBe` UTF8.fromString "<html><div class=\"hello\" id=\"hihi\">hi</div></html>"
      it "parses values" $ do
        res <- [parse|html
          div { class: theValue }
            | ha
         |]
        res `shouldBe` UTF8.fromString "<html><div class=\"VALUE\">ha</div></html>"
    context "UTF-8 Text" $ do
      it "parses simple utf-8" $ do
        res <- [parse|html
          div
            | 안녕
         |]
        res `shouldBe` UTF8.fromString "<html><div>안녕</div></html>"
    context "Simple Text" $ do
      it "parses simple tag" $ do
        res <- [parse|html
          div
            | Hello
         |]
        res `shouldBe` "<html><div>Hello</div></html>"
      it "parses simple variable" $ do
        res <- [parse|html
          div
            = theValue
         |]
        res `shouldBe` "<html><div>VALUE</div></html>"
      it "parses simple tag" $ do
        res <- [parse|html
          div
            | Hello
         |]
        res `shouldBe` "<html><div>Hello</div></html>"
      it "processes simple foreach statement" $ do
        res <- [parse|- foreach people -> name, title
          div
            p
              = name
            p
              = title
         |]
        res `shouldBe` "<div><p>A</p><p>B</p></div>"
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
  theValue = "VALUE" :: BS.ByteString
  people = [["A", "B"] :: [BS.ByteString]]
  trueStatement = True
  falseStatement = False
  greaterThan = (>) :: Integer -> Integer -> Bool
  three = 3 :: Integer
  four = 4 :: Integer

main :: IO ()
main = hspec spec
