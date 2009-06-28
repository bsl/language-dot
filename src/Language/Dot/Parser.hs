module Language.Dot.Parser
  (
    parseGraph
  )
  where

import Data.Char (toLower)

import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Language (emptyDef)
import Text.Parsec.Token

import Language.Dot.Syntax

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

parseGraph :: Parser Graph
parseGraph =
    (<?> "graph") $ do
    whiteSpaceL
    strictness   <- parseGraphStrictness
    directedness <- parseGraphDirectedness
    graphId      <- optionMaybe parseId
    statements   <- parseGraphStatementList
    eof
    return $ Graph strictness directedness graphId statements

parseGraphStrictness :: Parser GraphStrictness
parseGraphStrictness =
    (<?> "strictness") $
    fmap (maybe NotStrict (const Strict)) (optionMaybe (reservedL "strict"))

parseGraphDirectedness :: Parser GraphDirectedness
parseGraphDirectedness =
    (<?> "directedness") $
        (reservedL "graph"   >> return Undirected)
    <|> (reservedL "digraph" >> return Directed)

parseId :: Parser Id
parseId =
    (<?> "id") $
    fmap Id (identifierL <|> stringLiteralL <|> fmap show floatL <|> fmap show decimalL)

parseGraphStatementList :: Parser [Statement]
parseGraphStatementList =
    (<?> "statement list") $
    bracesL (many parseGraphStatement)

parseGraphStatement :: Parser Statement
parseGraphStatement =
    (<?> "statement") $ do
    statement <- parseNodeStatement
    optional semiL
    return statement

{-
    <|> parseEdgeStatement
    <|> parseAttributeStatement
    <|> parseAssignmentStatement
    <|> parseSubgraphStatement
-}

parseNodeStatement :: Parser Statement
parseNodeStatement =
    (<?> "node statement") $ do
    nodeId     <- parseNodeId
    attributes <- parseAttributeList
    return $ NodeStatement nodeId attributes

{-
parseEdgeStatement :: Parser Statement
parseEdgeStatement =
    (<?> "edge statement") $


parseAttributeStatement :: Parser Statement
parseAttributeStatement =
    (<?> "attribute statement") $


parseAssignmentStatement :: Parser Statement
parseAssignmentStatement =
    (<?> "assignment statement") $

parseSubgraphStatement :: Parser Statement
parseSubgraphStatement =
    (<?> "subgraph statement") $
-}

parseNodeId :: Parser NodeId
parseNodeId =
    (<?> "node ID") $ do
    nodeId <- parseId
    port   <- optionMaybe parsePort
    return $ NodeId nodeId port

parsePort :: Parser Port
parsePort =
    (<?> "port") $
        try parsePortC
    <|> try parsePortI

parsePortC :: Parser Port
parsePortC =
    (<?> "port (compass variant)") $ do
    colonL
    fmap PortC parseCompass

parsePortI :: Parser Port
parsePortI =
    (<?> "port (id variant)") $ do
    colonL
    portId  <- parseId
    compass <- optionMaybe (colonL >> parseCompass)
    return $ PortI portId compass

parseCompass :: Parser Compass
parseCompass =
    (<?> "compass") $ do
    x <- identifierL
    case stringToLower x of
      "n"  -> return CompassN
      "e"  -> return CompassE
      "s"  -> return CompassS
      "w"  -> return CompassW
      "ne" -> return CompassNE
      "nw" -> return CompassNW
      "se" -> return CompassSE
      "sw" -> return CompassSW
      _    -> parserFail $ "illegal compass value " ++ show x

parseAttributeList :: Parser [Attribute]
parseAttributeList =
    (<?> "attribute list") $
    bracketsL (many parseAttribute)

parseAttribute :: Parser Attribute
parseAttribute =
    (<?> "attribute") $ do
    id0 <- parseId
    id1 <- optionMaybe (charL '=' >> parseId)
    optional commaL
    return $ maybe (AttributeSetTrue id0) (AttributeSetValue id0) id1

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

charL :: Char -> Parser Char
charL = lexemeL . char

bracesL        :: Parser a -> Parser a
bracketsL      :: Parser a -> Parser a
colonL         :: Parser String
commaL         :: Parser String
decimalL       :: Parser Integer
floatL         :: Parser Double
identifierL    :: Parser String
lexemeL        :: Parser a -> Parser a
reservedL      :: String -> Parser ()
semiL          :: Parser String
stringLiteralL :: Parser String
whiteSpaceL    :: Parser ()

bracesL        = braces        lexer
bracketsL      = brackets      lexer
colonL         = colon         lexer
commaL         = comma         lexer
decimalL       = decimal       lexer
floatL         = float         lexer
identifierL    = identifier    lexer
lexemeL        = lexeme        lexer
reservedL      = reserved      lexer
semiL          = semi          lexer
stringLiteralL = stringLiteral lexer
whiteSpaceL    = whiteSpace    lexer

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

stringToLower :: String -> String
stringToLower = map toLower
