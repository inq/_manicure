{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE QuasiQuotes          #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE FlexibleContexts     #-}
module Manicure.Config where

import qualified Manicure.Route                 as Route
import qualified Manicure.Response              as Resi
import qualified Manicure.Parser                as P
import qualified Data.ByteString.Char8          as BS
import qualified Language.Haskell.TH.Syntax     as TS
import qualified Language.Haskell.TH.Quote      as TQ

import qualified Data.Map.Strict                as M
import Control.Applicative ((*>), (<*))
import Control.Monad (liftM2)

data Config = Config [(String, String)]

instance TS.Lift Config where
    lift (Config list) = [|
            M.fromList $ map (\(a, b) -> (BS.pack a, BS.pack b)) list
        |]

parseFile :: FilePath -> TS.Q TS.Exp
-- ^ Parse the configuration file
parseFile file_path = do
    TS.qAddDependentFile file_path
    s <- TS.qRunIO $ readFile file_path
    TQ.quoteExp parse s
  where
    parse = TQ.QuasiQuoter {
        TQ.quoteExp = quote_exp,
        TQ.quotePat = undefined,
        TQ.quoteType = undefined,
        TQ.quoteDec = undefined
      }
    quote_exp str = do
        case P.parseOnly parseData (BS.pack str) of
            Left err -> undefined
            Right tag -> [| tag |]
    parseData = do
        lines <- P.many parseLine
        return $ Config lines
    parseLine = do
        (P.many . P.char) '\n'
        key <- P.noneOf1 " =" <* (P.many . P.char) ' '
        P.char '='
        value <- (P.many . P.char) ' ' *> P.noneOf1 "\n"
        (P.many . P.char) '\n'
        return (BS.unpack key, BS.unpack value)
