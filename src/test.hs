module Main where

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

import Control.Monad (unless)

import Text.Parsec
import Text.Parsec.String

import Language.Dot.Parser
import Language.Dot.Syntax

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

main :: IO ()
main =
    testParser "parseId" parseId parseIdTests

parseIdTests :: [(String, Id)]
parseIdTests =
    [ ( "a"             , NameId    "a"      )
    , ( "A1"            , NameId    "A1"     )
    , ( "_2X"           , NameId    "_2X"    )
    , ( "\"\""          , StringId  ""       )
    , ( "\"\\\"\""      , StringId  "\""     )
    , ( "\"abc\""       , StringId  "abc"    )
    , ( "\"\\t\\r\\n\"" , StringId  "\t\r\n" )
    , ( ".0"            , FloatId   0.0      )
    , ( ".123"          , FloatId   0.123    )
    , ( "+.999"         , FloatId   0.999    )
    , ( "-.001"         , FloatId   (-0.001) )
    , ( "+.001"         , FloatId   0.001    )
    , ( "0.0"           , FloatId   0.0      )
    , ( "1.2"           , FloatId   1.2      )
    , ( "123.456"       , FloatId   123.456  )
    , ( "0"             , IntegerId 0        )
    , ( "+0"            , IntegerId 0        )
    , ( "-0"            , IntegerId 0        )
    , ( "123"           , IntegerId 123      )
    , ( "-123"          , IntegerId (-123)   )
    ]

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

testParser :: (Eq a, Show a) => String -> Parser a -> [(String, a)] -> IO ()
testParser name parser tests =
    help tests [] (0 :: Int) (0 :: Int)
  where
    help [] es np nf = do
        putStrLn $ name ++ ": " ++ show np ++ " passed, " ++ show nf ++ " failed"
        mapM_ (putStrLn . ("  "++)) (reverse es)
        unless (null es) (putStrLn "")
    help ((i,o):ts) es np nf =
        case parse' parser i of
          Left  _ -> help ts (makeFailureMessage name i o : es) np (succ nf)
          Right v ->
            if v /= o
              then help ts (makeFailureMessage' name i o v : es) np (succ nf)
              else help ts es (succ np) nf

makeFailureMessage :: (Show a) => String -> String -> a -> String
makeFailureMessage name i o =
    "(" ++ name ++ " " ++ show i ++ ")" ++
    " should have returned " ++ "(" ++ show o ++ ")"

makeFailureMessage' :: (Show a) => String -> String -> a -> a -> String
makeFailureMessage' name i o v =
    "(" ++ name ++ " " ++ show i ++ ")" ++
    " returned "  ++ "(" ++ show v ++ ")" ++
    ", expected " ++ "(" ++ show o ++ ")"

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

parse' :: Parser a -> String -> Either ParseError a
parse' p = parse p ""
