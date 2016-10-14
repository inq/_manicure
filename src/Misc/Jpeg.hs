module Misc.Jpeg
  ( Segment (..)
  , parse
  ) where

import qualified Data.ByteString.Lazy as BS
import qualified Misc.Jpeg.Segment as Segment
import Misc.Jpeg.Segment( Segment(..) )


parse :: BS.ByteString -> Maybe [Segment]
parse = Segment.parse
