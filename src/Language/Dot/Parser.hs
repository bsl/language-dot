module Language.Dot.Parser
  (
    parseGraph
  )
  where

import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Language (emptyDef)
import Text.Parsec.Token

import Language.Dot.Syntax

parseGraph :: Parser Graph
parseGraph = do
    whiteSpaceL
    strictness   <- parseStrictness
    directedness <- parseDirectedness
    graphId      <- optionMaybe parseId
    return $ Graph strictness directedness graphId []
  where
    parseStrictness =
        (<?> "strictness") $
        fmap (maybe NotStrict (const Strict)) (optionMaybe (reservedL "strict"))

    parseDirectedness =
        (<?> "directedness") $
            (reservedL "graph"   >> return Undirected)
        <|> (reservedL "digraph" >> return Directed)

parseId :: Parser Id
parseId =
    (<?> "id") $
    fmap Id (     identifierL
              <|> stringLiteralL
              <|> fmap show floatL
              <|> fmap show decimalL
            )

reservedL :: String -> Parser ()
reservedL = reserved lexer

whiteSpaceL :: Parser ()
whiteSpaceL = whiteSpace lexer

identifierL :: Parser String
identifierL = identifier lexer

stringLiteralL :: Parser String
stringLiteralL = stringLiteral lexer

floatL :: Parser Double
floatL = float lexer

decimalL :: Parser Integer
decimalL = decimal lexer

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
      , reservedOpNames = [ "->", "--", "=", "+" ]
      , reservedNames   = [ "digraph", "edge", "graph", "node", "strict", "subgraph"
                          , "e", "n", "ne", "nw", "s", "se", "sw", "w"
                          ]
      , caseSensitive   = False
      }
