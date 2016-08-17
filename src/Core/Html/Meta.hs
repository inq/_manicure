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
import Core.Html.Node

-- * Data types

data MetaNode
  = MStr ![Token]
  | MBts ![Token]
  | MMon ![Token]
  | MMap !String ![String] ![MetaNode]
  | MIf ![String] ![MetaNode]
  deriving Show

-- * Instances

instance TS.Lift MetaNode where
  lift (MStr es) =
    [| return $ UTF8.fromString $(return $
         (foldl TS.AppE (conv $ head es)) (map conv $ tail es))
     |]
  lift (MBts es) =
    [| return $(return $
         (foldl TS.AppE (conv $ head es)) (map conv $ tail es))
     |]
  lift (MMon as) =
    [| $(return $
         (foldl TS.AppE (conv $ head as) (map conv $ tail as)))
     |]
  lift (MMap vs v nodes) =
    [| BS.concat <$> (sequence $ concatMap
        (\($(return $ mkP v)) -> $(TS.lift nodes))
         $(return $ TS.VarE $ TS.mkName vs))
     |]
   where
    mkP (p : []) = TS.VarP $ TS.mkName p
    mkP (p : ps) = TS.ConP (TS.mkName p) $ map (TS.VarP . TS.mkName) ps
    mkP [] = error "empty pattern"
  lift (MIf attrs nodes) =
    [| case $(return $
              (foldl TS.AppE
                ((TS.VarE . TS.mkName . head) attrs)
                (map (TS.VarE . TS.mkName) (tail attrs)))) of
           True -> BS.concat <$> sequence $(TS.lift nodes)
           _ -> return ""
     |]

-- * Optimizer

optimize :: [MetaNode] -> [MetaNode]
optimize (MStr [TStr a] : MStr [TStr b] : res) = optimize $ MStr [TStr (a ++ b)] : res
optimize (MStr s@[TStr _] : res) = MStr s : optimize res
optimize (MMap vs v nodes : res) =
    (MMap vs v $ optimize nodes) : optimize res
optimize (MIf attrs nodes : res) =
    (MIf attrs $ optimize nodes) : optimize res
optimize (a : res) = a : optimize res
optimize [] = []

-- * Converter

conv :: Token -> TS.Exp
-- ^ Convert a token to a TS expression
conv (TStr s) = TS.LitE $ TS.StringL s
conv (TRef s) = TS.VarE $ TS.mkName s

convert :: Node -> [MetaNode]
-- ^ Convert a node to a list of meta nodes
convert (NTag name attrs nodes) = concat
  [ [MStr [TStr $ "<" ++ name]]
  , concatMap fromAttr attrs
  , [MStr [TStr ">"]]
  , concatMap convert nodes
  , [MStr [TStr $ "</" ++ name ++ ">"]]
  ]
convert (NMap vs v ns) = [ MMap vs v $ concatMap convert ns ]
convert (NIf c ns) = [ MIf c $ concatMap convert ns ]
convert (NStr t) = [ MStr t ]
convert (NBts t) = [ MBts t ]
convert (NMon t) = [ MMon t ]

fromAttr :: Attr -> [MetaNode]
-- ^ Convert an attr to a list of meta nodes
fromAttr (ABts s t) = [MStr [TStr $ " " ++ s ++ "=\""], MBts t, MStr [TStr "\""]]
fromAttr (AStr s t) = [MStr [TStr $ " " ++ s ++ "=\""], MStr t, MStr [TStr "\""]]
