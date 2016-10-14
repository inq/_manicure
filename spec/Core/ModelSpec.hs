{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}
module Core.ModelSpec where

import qualified Data.ByteString.Char8 as BS
import qualified Data.Map.Strict as M
import qualified GHC.Generics as GN
import qualified Misc.Json as Json
import Core.Database ((=:))
import Core.Model
import SpecHelper

data Post = Post
  { postId :: Maybe BS.ByteString
  , count :: Integer
  , email :: BS.ByteString
  , password :: BS.ByteString
  } deriving (GN.Generic)
instance Model Post

spec :: Spec
spec =
  describe "Core.ModelSpec" $
    context "Simple Text" $ do
      it "parses json with postId" $
        gToJson (GN.from dataWithPostId) `shouldBe`
          Json.JSObject (M.fromList
            [ ("count", Json.JSInt 3)
            , ("email", Json.JSString "hello@world.com")
            , ("password", Json.JSString "SHA256")
            , ("postId", Json.JSString "Hello")
            ])
      it "parses json without postId" $
        gToJson (GN.from dataWithoutPostId) `shouldBe`
          Json.JSObject (M.fromList
            [ ("count", Json.JSInt 2)
            , ("email", Json.JSString "admin@hello.com")
            , ("password", Json.JSString "MD5")
            ])
      it "parses bson with postId" $
        gToDocument (GN.from dataWithPostId) `shouldBe`
          [ "postId" =: ("Hello" :: BS.ByteString)
          , "count" =: (3 :: Int)
          , "email" =: ("hello@world.com" :: BS.ByteString)
          , "password" =: ("SHA256" :: BS.ByteString)
          ]
      it "parses bson without postId" $
        gToDocument (GN.from dataWithoutPostId) `shouldBe`
          [ "count" =: (2 :: Int)
          , "email" =: ("admin@hello.com" :: BS.ByteString)
          , "password" =: ("MD5" :: BS.ByteString)
          ]
     where
      dataWithPostId = Post (Just "Hello") 3 "hello@world.com" "SHA256"
      dataWithoutPostId = Post Nothing 2 "admin@hello.com" "MD5"

main :: IO ()
main = hspec spec
