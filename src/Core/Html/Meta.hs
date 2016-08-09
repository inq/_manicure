{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}

module Core.Html.Meta
  ( MetaNode(..)
  , optimize
  , convert
  ) where

import qualified Data.ByteString.Char8            as BS
import qualified Language.Haskell.TH.Syntax       as TS
import qualified Data.ByteString.UTF8             as UTF8
import qualified Core.ByteString                  as ByteString
import Core.Html.Node

-- * Data types

data MetaNode
  = MStr !String
  | MVal !String
  | MMon !String
  | MForeach !String ![String] ![MetaNode]
  | MIf ![String] ![MetaNode]

-- * Instances

instance TS.Lift MetaNode where
  lift (MStr a) = [| return $ UTF8.fromString a |]
  lift (MVal a) = [| return $ ByteString.convert $(return $ TS.VarE $ TS.mkName a) |]
  lift (MMon a) = [| $(return $ TS.VarE $ TS.mkName a) |]
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

-- * Optimizer

optimize :: [MetaNode] -> [MetaNode]
optimize (MStr a : MStr b : res) = optimize $ MStr (a ++ b) : res
optimize (MStr a : res) = MStr a : optimize res
optimize (MForeach vals vs nodes : res) =
    (MForeach vals vs $ optimize nodes) : optimize res
optimize (MIf attrs nodes : res) =
    (MIf attrs $ optimize nodes) : optimize res
optimize (a : res) = a : optimize res
optimize [] = []

-- * Converter

convert :: Node -> [MetaNode]
-- ^ Convert a node to a list of meta nodes
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
-- ^ Convert an attr to a list of meta nodes
fromAttr (Attr s t) = [MStr $ " " ++ s ++ "=\"", fromToken t, MStr "\""]

fromToken :: Token -> MetaNode
-- ^ Convert a token to a list of meta nodes
fromToken (TStr s) = MStr s
fromToken (TVal s) = MVal s
fromToken (TMon s) = MMon s
