{-# LANGUAGE CPP #-}

module Language.Dot.Parser
  (
    parseDotData
  , parseDotFile

#ifdef TEST
  , parseId
#endif
  )
  where

import Control.Applicative ((<$>), (<$), (<*>), (*>))
import Data.Char           (digitToInt, toLower)
import Data.List           (foldl')
import Data.Maybe          (fromMaybe)
import Numeric             (readFloat)
import System.IO           (readFile)

import Text.Parsec
import Text.Parsec.Combinator
import Text.Parsec.Language
import Text.Parsec.String
import Text.Parsec.Token

import Language.Dot.Syntax

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

parseDotData :: String -> Either ParseError Graph
parseDotData =
    parse parseDot "data" . preprocess

parseDotFile :: FilePath -> IO (Either ParseError Graph)
parseDotFile fp =
    fmap (parse parseDot fp . preprocess) (readFile fp)

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

preprocess :: String -> String
preprocess =
    unlines . map commentPoundLines . lines
  where
    commentPoundLines []         = []
    commentPoundLines line@(c:_) = if c == '#' then "// " ++ line else line

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

parseDot :: Parser Graph
parseDot =
    whiteSpace' >> parseGraph

parseGraph :: Parser Graph
parseGraph =
    Graph <$>
      "graph" <??>
          parseStrictness
      <*> parseDirectedness
      <*> optionMaybe parseId
      <*> parseStatementList

parseStrictness :: Parser GraphStrictness
parseStrictness =
    "graph strictness" <??> (Strict <$ reserved' "strict") <|> return NotStrict

parseDirectedness :: Parser GraphDirectedness
parseDirectedness =
    "graph directedness" <??>
        (reserved' "graph"   >> return Undirected)
    <|> (reserved' "digraph" >> return Directed)

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

parseStatementList :: Parser [Statement]
parseStatementList =
    "statement list" <??> braces' (parseStatement `endBy` optional semi')

parseStatement :: Parser Statement
parseStatement =
    "statement" <??>
    (   try parseEdgeStatement
    <|> try parseAttributeStatement
    <|> try parseAssignmentStatement
    <|> try parseSubgraphStatement
    <|> try parseNodeStatement
    )

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

parseNodeStatement :: Parser Statement
parseNodeStatement =
    NodeStatement <$> "node statement" <??> parseNodeId <*> parseAttributeList

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

parseEdgeStatement :: Parser Statement
parseEdgeStatement =
    EdgeStatement <$> "edge statement" <??> parseEntityList <*> parseAttributeList

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

parseAttributeStatement :: Parser Statement
parseAttributeStatement =
    AttributeStatement <$> "attribute statement" <??> parseAttributeStatementType <*> parseAttributeList

parseAttributeStatementType :: Parser AttributeStatementType
parseAttributeStatementType =
    "attribute statement type" <??>
        (reserved' "graph" >> return GraphAttributeStatement)
    <|> (reserved' "node"  >> return NodeAttributeStatement)
    <|> (reserved' "edge"  >> return EdgeAttributeStatement)

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

parseAssignmentStatement :: Parser Statement
parseAssignmentStatement =
    AssignmentStatement <$> "assignment statement" <??> parseId <*> (reservedOp' "=" *> parseId)

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

parseSubgraphStatement :: Parser Statement
parseSubgraphStatement =
    SubgraphStatement <$> "subgraph statement" <??> parseSubgraph

parseSubgraph :: Parser Subgraph
parseSubgraph =
    "subgraph" <??> try parseNewSubgraph <|> parseSubgraphRef

parseNewSubgraph :: Parser Subgraph
parseNewSubgraph =
    NewSubgraph <$> "new subgraph" <??> (optional (reserved' "subgraph") *> optionMaybe parseId) <*> parseStatementList

parseSubgraphRef :: Parser Subgraph
parseSubgraphRef =
    SubgraphRef <$> "subgraph ref" <??> (reserved' "subgraph" *> parseId)

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

parseEntityList :: Parser [Entity]
parseEntityList =
    (:) <$> "entity list" <??> parseEntity <*> many1 (parseEdgeOp *> parseEntity)

parseEntity :: Parser Entity
parseEntity =
    "entity" <??> try parseENodeId <|> try parseESubgraph

parseENodeId :: Parser Entity
parseENodeId =
    ENodeId <$> "entity node id" <??> parseNodeId

parseESubgraph :: Parser Entity
parseESubgraph =
    ESubgraph <$> "entity subgraph" <??> parseSubgraph

parseEdgeOp :: Parser ()
parseEdgeOp =
    "edge operator" <??> try (reservedOp' "--") <|> reservedOp' "->"

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

parseNodeId :: Parser NodeId
parseNodeId =
    NodeId <$> "node id" <??> parseId <*> optionMaybe parsePort

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

parsePort :: Parser Port
parsePort =
    "port" <??> try parsePortC <|> parsePortI

parsePortC :: Parser Port
parsePortC =
    PortC <$> "port (compass)" <??> (colon' *> parseCompass)

parsePortI :: Parser Port
parsePortI =
    PortI <$> "port (id)" <??> (colon' *> parseId) <*> optionMaybe (colon' *> parseCompass)

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

parseCompass :: Parser Compass
parseCompass =
    "compass" <??> fmap convert identifier' >>= maybe err return
  where
    err = parserFail "invalid compass value"
    convert =
        flip lookup table . stringToLower
      where
        table =
          [ ("n",  CompassN),  ("e",  CompassE),  ("s",  CompassS),  ("w",  CompassW)
          , ("ne", CompassNE), ("nw", CompassNW), ("se", CompassSE), ("sw", CompassSW)
          ]

parseAttributeList :: Parser [Attribute]
parseAttributeList =
    "attribute list" <??> brackets' (parseAttribute `sepBy` optional comma') <|> return []

parseAttribute :: Parser Attribute
parseAttribute =
    "attribute" <??> do
    id0 <- parseId
    id1 <- optionMaybe (reservedOp' "=" >> parseId)
    return $ maybe (AttributeSetTrue id0) (AttributeSetValue id0) id1

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

parseId :: Parser Id
parseId =
    "id" <??>
    (   try parseNameId
    <|> try parseStringId
    <|> try parseFloatId
    <|> try parseIntegerId
    )

parseNameId :: Parser Id
parseNameId =
    NameId <$> "name" <??> identifier'

parseStringId :: Parser Id
parseStringId =
    StringId <$> "string literal" <??> stringLiteral'

parseIntegerId :: Parser Id
parseIntegerId =
    IntegerId <$> "integer" <??> integer'

parseFloatId :: Parser Id
parseFloatId =
    lexeme' $
    "float" <??> do
    s <- parseSign
    l <- fmap (fromMaybe 0) (optionMaybe parseNatural)
    char '.'
    r <- many1 digit
    maybe err return (make s (show l ++ "." ++ r))
  where
    err = parserFail "invalid float value"
    make s f =
        case readFloat f of
          [(v,"")] -> (Just . FloatId . s) v
          _        -> Nothing

parseSign :: (Num a) => Parser (a -> a)
parseSign =
    "sign" <??>
        (char '-' >> return negate)
    <|> (char '+' >> return id)
    <|> return id

-- | Non-lexeme version for parsing the natural part of a float.
parseNatural :: Parser Integer
parseNatural =
    "natural" <??>
        (char '0' >> return 0)
    <|> (convert <$> many1 digit)
  where
    convert = foldl' (\acc d -> 10 * acc + fromIntegral (digitToInt d)) 0

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

braces'        :: Parser a -> Parser a
brackets'      :: Parser a -> Parser a
colon'         :: Parser String
comma'         :: Parser String
identifier'    :: Parser String
integer'       :: Parser Integer
lexeme'        :: Parser a -> Parser a
reserved'      :: String -> Parser ()
reservedOp'    :: String -> Parser ()
semi'          :: Parser String
stringLiteral' :: Parser String
whiteSpace'    :: Parser ()

braces'        = braces        lexer
brackets'      = brackets      lexer
colon'         = colon         lexer
comma'         = comma         lexer
identifier'    = identifier    lexer
integer'       = integer       lexer
lexeme'        = lexeme        lexer
reserved'      = reserved      lexer
reservedOp'    = reservedOp    lexer
semi'          = semi          lexer
stringLiteral' = stringLiteral lexer
whiteSpace'    = whiteSpace    lexer

lexer :: TokenParser ()
lexer =
    makeTokenParser dotDef
  where
    dotDef = emptyDef
      { commentStart    = "/*"
      , commentEnd      = "*/"
      , commentLine     = "//"
      , nestedComments  = True
      , identStart      = letter   <|> char '_'
      , identLetter     = alphaNum <|> char '_'
      , opStart         = oneOf "-="
      , opLetter        = oneOf ">-"
      , reservedOpNames = ["->", "--", "="]
      , reservedNames   = ["digraph", "edge", "graph", "node", "strict", "subgraph"]
      , caseSensitive   = False
      }

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

(<??>) :: Monad m => String -> ParsecT s u m a -> ParsecT s u m a
(<??>) = flip (<?>)

stringToLower :: String -> String
stringToLower = map toLower
