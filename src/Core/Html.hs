{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE EmptyCase         #-}
{-# LANGUAGE FlexibleContexts  #-}
module Core.Html where

import qualified Data.ByteString.Char8            as BS
import qualified Language.Haskell.TH.Quote        as TQ
import qualified Language.Haskell.TH.Syntax       as TS
import qualified Core.ByteString                  as ByteString
import qualified Data.ByteString.UTF8             as UTF8
import qualified Core.Parser                      as P
import Control.Applicative ((<|>))

data Attr = Dash !String ![Attr]
          | Attr !String !String
          deriving Show
data Node = Tag !String ![Attr] ![Node]
          | Text !String
          | Value !String
          | Foreach !String ![String] ![Node]
          | Render !String
          | If ![String] ![Node]
          deriving Show
data Html = Html ![Node]
data Status = Child | Sibling | Parent
          deriving Show

instance TS.Lift Html where
    lift (Html nodes) = [| BS.concat nodes |]

instance TS.Lift Node where
    lift (Tag string attrs nodes) = [|
          BS.concat ([$(TS.lift $ "<" ++ string ++ procAttrs attrs ++ ">")]
            ++ $(TS.lift nodes)
            ++ [$(TS.lift $ "</" ++ string ++ ">")]
            )
        |]
      where
        procAttrs (Attr name value : remainders) =
            " " ++ name ++ "=" ++ value ++ procAttrs remainders
        procAttrs (_ : _) = error "procAttrs: Dash is not allowed"
        procAttrs [] = ""
    lift (Foreach vals vs nodes) = [|
          BS.concat $
            map
              (\($(return $ (TS.ListP $ map (TS.VarP . TS.mkName) vs))) -> BS.concat nodes)
              $(return $ TS.VarE $ TS.mkName vals)
        |]
    lift (Render fileName) = [|
          $(parseFile fileName)
        |]
    lift (If attrs nodes) = [|
          case $(return $
                  (foldl (\a b -> TS.AppE a b)
                    ((TS.VarE . TS.mkName . head) attrs)
                    (map (TS.VarE . TS.mkName) (tail attrs)))) of
            True -> BS.concat nodes
            _ -> ""
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
        case P.parseOnly parseNode (UTF8.fromString str) of
            Right tag -> [| tag |]
            Left _    -> undefined

parseLine :: P.Parser (Int, Node)
parseLine = do
    indent <- parseIndent
    c <- P.peekChar'
    tag <- case c of
        '|' -> textNode
        '=' -> valueNode
        '-' -> commandNode
        _ -> tagNode
    _ <- P.char '\n'
    return (indent, tag)
  where
    valueNode = do
        P.anyChar *> P.skipSpace
        val <- P.noneOf "\n"
        return $ Value $ UTF8.toString val
    textNode = P.anyChar *> P.skipSpace *> (Text <$> UTF8.toString <$> P.noneOf "\n")
    commandNode = do
        c <- P.anyChar *> P.skipSpace *> P.peekChar'
        case c of
            'r' -> renderNode
            'i' -> ifNode
            'f' -> foreachNode
            c' -> error $ "unexpected char(" ++ [c'] ++ ")"
      where
        renderNode = P.string "render" *> P.skipSpace *> (Render <$> UTF8.toString <$> P.noneOf "\n")
        ifNode = P.string "if" *> P.skipSpace *> (
            If
            <$> (map UTF8.toString <$> (P.sepBy (P.spaces *> P.noneOf " \n") $ P.char ' '))
            <*> return []
          )
        foreachNode = P.string "foreach" *> P.skipSpace *> (
            Foreach
            <$> UTF8.toString <$> P.noneOf " "
            <*> (map UTF8.toString <$>
                  (P.string " -> " *>
                    (P.sepBy (P.spaces *> P.noneOf " ,\n") $ P.char ',')))
            <*> return []
          )
    tagNode = Tag
        <$> UTF8.toString <$> (P.noneOf " \n")
        <*> (P.try parseArgs <|> return [])
        <*> return []
      where
        parseArgs = P.token '{' *> (P.sepBy parseArg $ P.token ',') <* P.char '}'
        parseArg = Attr
            <$> (UTF8.toString <$> (P.noneOf " :"))
            <*> (UTF8.toString <$> (P.token ':' *> P.noneOf1 ",}"))

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
    replace (Foreach vals val _) = Foreach vals val
    replace (Tag name attr _) = Tag name attr
    replace (If args _) = If args
    replace (Render _) = error "indentation error"
    replace (Text _) = error "indentation error"
    replace (Value _) = error "indentation error"
buildTree []  =
    (0, [], [])

parseNode :: P.Parser Html
-- ^ The main parser
parseNode = do
    nodes <- P.many parseLine
    let (_, res, _) = buildTree nodes
    return (Html res)
