module Language.Dot.Parser
  (
    parseDotContents
  )
  where

import Control.Applicative ((<$>), (<$), (<*>), (<*), (*>))
import Data.Char           (toLower)

import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Language (emptyDef)
import Text.Parsec.Token
import Text.Parsec.Combinator

import Language.Dot.Syntax

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

parseDotContents :: String -> Either ParseError Graph
parseDotContents = parse' parseDot . preprocess

parse' :: Parser a -> String -> Either ParseError a
parse' p = parse p "DOT"

preprocess :: String -> String
preprocess =
    unlines . map commentPoundLines . lines
  where
    commentPoundLines []         = []
    commentPoundLines line@(c:_) = if c == '#' then '/':'/':line else line

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

parseDot :: Parser Graph
parseDot = whiteSpace' >> parseGraph

parseGraph :: Parser Graph
parseGraph =
    Graph <$>
      "graph" <??>
          parseGraphStrictness
      <*> parseGraphDirectedness
      <*> optionMaybe parseId
      <*> parseGraphStatementList

parseGraphStrictness :: Parser GraphStrictness
parseGraphStrictness =
    "graph strictness" <??> (Strict <$ reserved' "strict") <|> return NotStrict

parseGraphDirectedness :: Parser GraphDirectedness
parseGraphDirectedness =
    "graph directedness" <??>
        (reserved' "graph"   >> return Undirected)
    <|> (reserved' "digraph" >> return Directed)

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

parseGraphStatementList :: Parser [Statement]
parseGraphStatementList =
    "statement list" <??> braces' (many parseGraphStatement)

parseGraphStatement :: Parser Statement
parseGraphStatement =
    "statement" <??>
    (   try parseNodeStatement
    <|> try parseEdgeStatement
    <|> try parseAttributeStatement
    <|> try parseAssignmentStatement
    <|> try parseSubgraphStatement
    ) <* optional semi'

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
    "subgraph" <??> reserved' "subgraph" *> (try parseNewSubgraph <|> try parseSubgraphRef)

parseNewSubgraph :: Parser Subgraph
parseNewSubgraph =
    NewSubgraph <$> "new subgraph" <??> optionMaybe parseId <*> parseGraphStatementList

parseSubgraphRef :: Parser Subgraph
parseSubgraphRef =
    SubgraphRef <$> "subgraph ref" <??> parseId

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

parseEntityList :: Parser [Entity]
parseEntityList =
    "entity list" <??> sepBy parseEntity parseEdgeOp

parseEntity :: Parser Entity
parseEntity =
    "entity" <??> try parseENodeId <|> parseESubgraph

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
    convert =
        flip lookup table . stringToLower
      where
        table =
          [ ("n",  CompassN)
          , ("e",  CompassE)
          , ("s",  CompassS)
          , ("w",  CompassW)
          , ("ne", CompassNE)
          , ("nw", CompassNW)
          , ("se", CompassSE)
          , ("sw", CompassSW)
          ]
    err = parserFail "invalid compass value"

parseAttributeList :: Parser [Attribute]
parseAttributeList =
    "attribute list" <??> brackets' (many parseAttribute)

parseAttribute :: Parser Attribute
parseAttribute =
    "attribute" <??> do
    id0 <- parseId
    id1 <- optionMaybe (reservedOp' "=" >> parseId)
    optional comma'
    return $ maybe (AttributeSetTrue id0) (AttributeSetValue id0) id1

parseId :: Parser Id
parseId =
    Id <$> "id" <??>
    (   identifier'
    <|> stringLiteral'
    <|> try (fmap show float')
    <|> fmap show decimal'
    )

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

braces'        :: Parser a -> Parser a
brackets'      :: Parser a -> Parser a
colon'         :: Parser String
comma'         :: Parser String
decimal'       :: Parser Integer
float'         :: Parser Double
identifier'    :: Parser String
reserved'      :: String -> Parser ()
reservedOp'    :: String -> Parser ()
semi'          :: Parser String
stringLiteral' :: Parser String
whiteSpace'    :: Parser ()

braces'        = braces        lexer
brackets'      = brackets      lexer
colon'         = colon         lexer
comma'         = comma         lexer
decimal'       = decimal       lexer
float'         = float         lexer
identifier'    = identifier    lexer
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
      , identStart      = letter   <|> char '_'
      , identLetter     = alphaNum <|> char '_'
      , nestedComments  = True
      , reservedOpNames = ["->", "--", "=", "+"]
      , reservedNames   = ["digraph", "edge", "graph", "node", "strict", "subgraph"]
      , caseSensitive   = False
      }

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

(<??>) :: Monad m => String -> ParsecT s u m a -> ParsecT s u m a
(<??>) = flip (<?>)

stringToLower :: String -> String
stringToLower = map toLower
