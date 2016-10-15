{-# LANGUAGE OverloadedStrings #-}
module Core.Request.ContentSpec where

import qualified Misc.Parser as P
import Core.Request.Content
import SpecHelper

spec :: Spec
spec =
  describe "Core.Request.ContentSpec" $ do
    context "Simple parsing" $ do
      it "parses Content-Disposition" $ do
        let contDisp = "form-data; name=\"data\"; filename=\"theFile.png\""
        P.parseOnly parseContDisp contDisp `shouldBe`
          Right (MkContDisp FormData (Just "data") (Just "theFile.png"))

main :: IO ()
main = hspec spec
