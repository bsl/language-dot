{-# LANGUAGE CPP #-}

module Language.Dot.Parser
  (
    parseDot
#ifdef TEST
  , parsePort
  , parseCompass
  , parseAttribute
  , parseId
#endif
  )
  where

import Control.Applicative ((<$>), (<$), (<*>), (*>))
import Data.Char           (digitToInt, toLower)
import Data.List           (foldl')
import Data.Maybe          (fromMaybe)
import Numeric             (readFloat)

import Text.Parsec
import Text.Parsec.Combinator
import Text.Parsec.Language
import Text.Parsec.String
import Text.Parsec.Token

import Language.Dot.Syntax

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

parseDot :: String -> Either ParseError Graph
parseDot =
    parse (whiteSpace' >> parseGraph) "data" . preprocess

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

preprocess :: String -> String
preprocess =
    unlines . map commentPoundLines . lines
  where
    commentPoundLines []         = []
    commentPoundLines line@(c:_) = if c == '#' then "// " ++ line else line

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

parseGraph :: Parser Graph
parseGraph =
    Graph <$>
      "graph" <??>
          parseGraphStrictness
      <*> parseGraphDirectedness
      <*> optionMaybe parseId
      <*> parseStatementList

parseGraphStrictness :: Parser GraphStrictness
parseGraphStrictness =
    "graph strictness" <??> (StrictGraph <$ reserved' "strict") <|> return UnstrictGraph

parseGraphDirectedness :: Parser GraphDirectedness
parseGraphDirectedness =
    "graph directedness" <??>
        (reserved' "graph"   >> return UndirectedGraph)
    <|> (reserved' "digraph" >> return DirectedGraph)

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
    <|>     parseNodeStatement
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
    (:) <$> "entity list" <??> parseEntity True <*> many1 (parseEntity False)

parseEntity :: Bool -> Parser Entity
parseEntity first =
    "entity" <??> try (parseENodeId first) <|> parseESubgraph first

parseENodeId :: Bool -> Parser Entity
parseENodeId first =
    ENodeId <$> "entity node id" <??>
    (if first then return NoEdge else parseEdgeType) <*> parseNodeId

parseESubgraph :: Bool -> Parser Entity
parseESubgraph first =
    ESubgraph <$> "entity subgraph" <??>
    (if first then return NoEdge else parseEdgeType) <*> parseSubgraph

parseEdgeType :: Parser EdgeType
parseEdgeType =
    "edge operator" <??>
        try (reservedOp' "->" >> return DirectedEdge)
    <|>     (reservedOp' "--" >> return UndirectedEdge)

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

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

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
    <|>     parseIntegerId
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

-- | DOT allows floating point numbers having no whole part like @.123@, but
--   Parsec 'float' does not accept them.
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

-- | Non-'lexeme' variant of 'natural' for parsing the natural part of a float.
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
      , opLetter        = oneOf ""
      , reservedOpNames = ["->", "--", "="]
      , reservedNames   = ["digraph", "edge", "graph", "node", "strict", "subgraph"]
      , caseSensitive   = False
      }

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

-- | Like '<?>', but used /before/ parser code.
(<??>) :: Monad m => String -> ParsecT s u m a -> ParsecT s u m a
(<??>) = flip (<?>)

stringToLower :: String -> String
stringToLower = map toLower
