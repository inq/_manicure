{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE EmptyCase         #-}
{-# LANGUAGE FlexibleContexts  #-}
module Manicure.Html where

import qualified Data.ByteString.Char8            as BS
import qualified Language.Haskell.TH.Quote        as TQ
import qualified Language.Haskell.TH.Syntax       as TS
import qualified Manicure.ByteString              as ByteString
import qualified Data.List                        as DL
import qualified Data.ByteString.UTF8             as UTF8
import qualified Manicure.Parser                  as P
import Control.Applicative ((*>), (<*), (<$>), (<*>), (<|>))
import Control.Monad

data Attr = Dash String [Attr]
          | Attr String String
          deriving Show
data Node = Tag String [Attr] [Node]
          | Text String
          | Value String
          | Foreach String [String] [Node]
          deriving Show
data Status = Child | Sibling | Parent
          deriving Show

instance TS.Lift Node where
    lift (Tag string attrs nodes) = [| 
          BS.concat ([$(TS.lift $ "<" ++ string ++ print attrs ++ ">")] 
            ++ $(TS.lift nodes)
            ++ [$(TS.lift $ "</" ++ string ++ ">")]
            )
        |]
      where
        print (Attr name value : attrs) = 
            " " ++ name ++ "=" ++ value ++ print attrs
        print [] = ""
    lift (Foreach vals vs nodes) = [|
          BS.concat $ 
            map
              (\($(return $ (TS.ListP $ map (TS.VarP . TS.mkName) vs))) -> BS.concat nodes)
              $(return $ TS.VarE $ TS.mkName vals) 
        |]
       
    lift (Text a) = [| UTF8.fromString a |]
    lift (Value a) = [| ByteString.convert $(return $ TS.VarE $ TS.mkName a) |]

instance TS.Lift Status where
    lift Child   = [| Child |]
    lift Sibling = [| Sibling |]
    lift Parent  = [| Parent |]

parseFile :: FilePath -> TS.Q TS.Exp
parseFile filePath = do
    TS.qAddDependentFile path
    s <- TS.qRunIO $ readFile path
    TQ.quoteExp parse s
  where
    path = "views/" ++ filePath

parse :: TQ.QuasiQuoter
parse = TQ.QuasiQuoter {
        TQ.quoteExp = quoteExp,
        TQ.quotePat = undefined,
        TQ.quoteType = undefined,
        TQ.quoteDec = undefined
    }
  where
    quoteExp str = do
        case P.parseOnly parseNode (BS.pack str) of
            Left err -> undefined
            Right tag -> [| tag |]

parseLine :: P.Parser (Int, Node)
parseLine = do
    nextIndent <- parseIndent
    tag <- valueNode <|> textNode <|> mapNode <|> tagNode
    P.char '\n'
    return (nextIndent, tag)
  where
    status indent nextIndent
        | indent > nextIndent = Child
        | indent < nextIndent = Parent
        | otherwise = Sibling
    tagNode = Tag
        <$> liftM BS.unpack (P.noneOf " \n")
        <*> (P.try parseArgs <|> return [])
        <*> return []
      where
        parseArgs = P.token '{' *> (P.sepBy parseArg $ P.token ',') <* P.char '}'
        parseArg = Attr
            <$> liftM BS.unpack (P.noneOf " :")
            <*> liftM BS.unpack (P.token ':' *> P.noneOf1 ",}")
    mapNode = Foreach
        <$> liftM BS.unpack (P.string "- foreach " *> P.noneOf " ")
        <*> liftM (map BS.unpack) (P.string " -> " *> (P.sepBy (P.spaces *> P.noneOf " ,\n") $ P.char ','))
        <*> return []
    valueNode = Value <$> liftM BS.unpack (P.string "= " *> P.noneOf "\n")
    textNode = Text <$> liftM BS.unpack (P.string "| " *> P.noneOf "\n")

parseIndent :: P.Parser Int
parseIndent = fmap sum $ P.many (
        (P.char ' ' >> return 1) <|> 
        (P.char '\t' >> fail "tab charactor is not allowed")
      )

buildTree :: [(Int, Node)] -> (Int, [Node], [(Int, Node)])
buildTree ((indent, node) : rest)
    | indent < next = buildTree $ (indent, replace node res) : remaining
    | indent > next = (indent, [node], rest)
    | otherwise  = (indent, (node) : res, remaining)
  where
    (next, res, remaining) = buildTree rest
    replace (Foreach vals val _) res = Foreach vals val res
    replace (Tag name attr _) res    = Tag name attr res
    replace (Text _) res = error "indentation error"
    replace (Value _) res = error "indentation error"
buildTree []  = 
    (0, [], [])

parseNode :: P.Parser Node
-- ^ The main parser
parseNode = do
    nodes <- P.many parseLine
    let (_, res, _) = buildTree nodes
    return $ head res
