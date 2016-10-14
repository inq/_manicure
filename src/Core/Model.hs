{-# LANGUAGE ScopedTypeVariables, ExplicitNamespaces #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances, OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
module Core.Model where

import qualified Data.ByteString.Char8            as BS
import qualified GHC.Generics                     as GN
import qualified Data.Char                        as C
import qualified Data.Typeable                    as T
import qualified Misc.Json                        as Json
import qualified Data.Map.Strict                  as M
import qualified Database.MongoDB                 as Mongo
import Core.Database ()
import GHC.Generics ((:*:)(..))
import Database.MongoDB ((=:))

data ModelInfo a = ModelInfo
  { table :: {-# UNPACK #-} !BS.ByteString
  , fields :: ![(BS.ByteString, T.TypeRep)]
  } deriving (Show)

class GModel f where
-- ^ Generic model
    gFields :: f a -> [(BS.ByteString, T.TypeRep)]
    gToJson :: f a -> Json.Json
    gToDocument :: f a -> Mongo.Document

instance GModel GN.U1 where
-- ^ Constructor without fields
    gFields _ = []
    gToJson _ = Json.JSArray []
    gToDocument _ = []
instance (GModel a, GModel b) => GModel (a :*: b) where
-- ^ Products
    gFields ~(a :*: b) = gFields a ++ gFields b
    gToJson ~(a :*: b) = Json.JSObject $ M.union m1 m2
        where
            Json.JSObject m1 = gToJson a
            Json.JSObject m2 = gToJson b
    gToDocument ~(a :*: b) = gToDocument a ++ gToDocument b
instance
  ( GN.Selector c
  , T.Typeable t
  , Mongo.Val t
  , Json.ToJson t
  ) => GModel (GN.M1 GN.S c (GN.K1 GN.R t)) where
-- ^ Record selector
    gFields s = [(BS.pack $ GN.selName s, T.typeOf (undefined :: t))]
    gToJson s = Json.JSObject $
        case gToJson $ GN.unM1 s of
            Json.JSNil -> M.empty
            val        -> M.fromList [(BS.pack $ GN.selName s, val)]
    gToDocument s =
        case Mongo.val (GN.unK1 $ GN.unM1 s) of
            Mongo.Null -> []
            val        -> [(BS.pack $ GN.selName s) =: val]

instance (Json.ToJson a) => GModel (GN.K1 GN.R a) where
-- ^ Parameter (Par ==> Rec)
    gFields _ = undefined
    gToJson s = Json.toJson $ GN.unK1 s
    gToDocument _ = undefined
instance (GModel f) => GModel (GN.M1 GN.C c f) where
-- ^ Constructor
    gFields = gFields . GN.unM1
    gToJson = gToJson . GN.unM1
    gToDocument = gToDocument . GN.unM1
instance (GModel f) => GModel (GN.M1 GN.D c f) where
-- ^ Datatype
    gFields = gFields . GN.unM1
    gToJson = gToJson . GN.unM1
    gToDocument = gToDocument . GN.unM1

class CollectionName f where
    collectionName :: f p -> String
instance (GN.Datatype c) => CollectionName (GN.D1 c f) where
    collectionName = GN.datatypeName

class
  ( GN.Generic a
  , CollectionName (GN.Rep a)
  , GModel (GN.Rep a)
  ) => Model a where
    modelInfo :: ModelInfo a
    modelInfo = ModelInfo
      { table = BS.pack $ map C.toLower $ collectionName $ GN.from (undefined :: a)
      , fields = gFields $ GN.from (undefined :: a)
      }
    toDocument :: a -> Mongo.Document
    toDocument = gToDocument . GN.from
