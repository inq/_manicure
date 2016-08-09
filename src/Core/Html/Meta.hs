{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}

module Core.Html.Meta where

import qualified Data.ByteString.Char8            as BS
import qualified Language.Haskell.TH.Syntax       as TS
import qualified Data.ByteString.UTF8             as UTF8
import qualified Core.ByteString                  as ByteString
import Core.Html.Node

data MetaNode
  = MStr !String
  | MVal !String
  | MForeach !String ![String] ![MetaNode]
  | MIf ![String] ![MetaNode]

instance TS.Lift MetaNode where
  lift (MStr a) = [| return $ UTF8.fromString a |]
  lift (MVal a) = [| return $ ByteString.convert $(return $ TS.VarE $ TS.mkName a) |]
  lift (MForeach vals vs nodes) =
    [| BS.concat <$> (sequence $ concatMap
        (\($(return $ (TS.ListP $ map (TS.VarP . TS.mkName) vs)))
            -> $(TS.lift nodes))
         $(return $ TS.VarE $ TS.mkName vals))
     |]
  lift (MIf attrs nodes) =
    [| case $(return $
              (foldl (\a b -> TS.AppE a b)
              ((TS.VarE . TS.mkName . head) attrs)
              (map (TS.VarE . TS.mkName) (tail attrs)))) of
           True -> BS.concat <$> sequence $(TS.lift nodes)
           _ -> return ""
     |]

convert :: Node -> [MetaNode]
convert (NTag name attrs nodes) = concat
  [ [MStr $ "<" ++ name]
  , concatMap fromAttr attrs
  , [MStr ">"]
  , concatMap convert nodes
  , [MStr $ "</" ++ name ++ ">"]
  ]
convert (NForeach vals vs ns) = [ MForeach vals vs $ concatMap convert ns ]
convert (NIf c ns) = [ MIf c $ concatMap convert ns ]
convert (NText t) = [ fromToken t ]

fromAttr :: Attr -> [MetaNode]
fromAttr (Attr s t) = [MStr $ " " ++ s ++ "=\"", fromToken t, MStr "\""]

fromToken :: Token -> MetaNode
fromToken (TStr s) = MStr s
fromToken (TVal s) = MVal s
