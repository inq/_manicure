module Misc.JpegSpec where

import qualified Data.ByteString.Lazy as BSL
import Misc.Jpeg
import SpecHelper

spec :: Spec
spec =
  describe "Misc.Jpeg" $ do
    context "Jpeg" $ do
      it "read the header" $ do
        s <- BSL.readFile "spec/Resources/test.jpg"
        print $ parse s
