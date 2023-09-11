module Main (main) where

import Control.Monad (unless, foldM)
import Data.Char     (toLower, toUpper)
import System.Exit   (exitSuccess, exitFailure)

import Text.Parsec
import Text.Parsec.String

import Language.Dot.Parser
import Language.Dot.Syntax

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

main :: IO ()
main = do
    (np,nf) <- foldM (\(p,f) t -> t >>= (\(p',f') -> return (p+p', f+f'))) (0,0)
               [
                 testParser "parsePort"      parsePort      parsePortTests
               , testParser "parseCompass"   parseCompass   parseCompassTests
               , testParser "parseAttribute" parseAttribute parseAttributeTests
               , testParser "parseId"        parseId        parseIdTests
               ]
    unless (nf == 0) $ do
      putStrLn ("Final results: "
                <> show np <> " tests passed but "
                <> show nf <> " failed.")
      exitFailure
    putStrLn $ "All " <> show np <> " tests passed."
    exitSuccess


-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

parsePortTests :: [(String, Port)]
parsePortTests =
    [ ( ":\"x\""          , PortI (StringId  "x"       ) Nothing          )
    , ( ":\"\\t\\\"\":nw" , PortI (StringId  "\\t\""   ) (Just CompassNW) )
    , ( ":-.0004"         , PortI (FloatId   (-0.0004) ) Nothing          )
    , ( ":-1.23:sE"       , PortI (FloatId   (-1.23)   ) (Just CompassSE) )
    , ( ":123"            , PortI (IntegerId 123       ) Nothing          )
    , ( ":123:NE"         , PortI (IntegerId 123       ) (Just CompassNE) )
    , ( ":__2xYz"         , PortI (NameId    "__2xYz"  ) Nothing          )
    , ( ":__2xYz:S"       , PortI (NameId    "__2xYz"  ) (Just CompassS)  )
    , ( ":n"              , PortC CompassN  )
    , ( ":SE"             , PortC CompassSE )
    ]

parseCompassTests :: [(String, Compass)]
parseCompassTests =
    concat
      [ [ (t, CompassN)  | t <- allCaps "n"  ]
      , [ (t, CompassE)  | t <- allCaps "e"  ]
      , [ (t, CompassS)  | t <- allCaps "s"  ]
      , [ (t, CompassW)  | t <- allCaps "w"  ]
      , [ (t, CompassNE) | t <- allCaps "ne" ]
      , [ (t, CompassNW) | t <- allCaps "nw" ]
      , [ (t, CompassSE) | t <- allCaps "se" ]
      , [ (t, CompassSW) | t <- allCaps "sw" ]
      ]

parseAttributeTests :: [(String, Attribute)]
parseAttributeTests =
    [ ( "a"                      , AttributeSetTrue  (NameId "a")                           )
    , ( "a=b"                    , AttributeSetValue (NameId "a")       (NameId "b")        )
    , ( "-.003\t=\r\n  _xYz123_" , AttributeSetValue (FloatId (-0.003)) (NameId "_xYz123_") )
    , ( "\"\\t\\\"\"  =-123"     , AttributeSetValue (StringId "\\t\"") (IntegerId (-123))  )
    ]

parseIdTests :: [(String, Id)]
parseIdTests =
    [ ( "a"             , NameId    "a"         )
    , ( "A1"            , NameId    "A1"        )
    , ( "_2X"           , NameId    "_2X"       )
    , ( "\"\""          , StringId  ""          )
    , ( "\"\\t\\r\\n\"" , StringId  "\\t\\r\\n" )
    , ( ".0"            , FloatId   0.0         )
    , ( ".123"          , FloatId   0.123       )
    , ( "+.999"         , FloatId   0.999       )
    , ( "-.001"         , FloatId   (-0.001)    )
    , ( "+.001"         , FloatId   0.001       )
    , ( "0.0"           , FloatId   0.0         )
    , ( "1.2"           , FloatId   1.2         )
    , ( "123.456"       , FloatId   123.456     )
    , ( "0"             , IntegerId 0           )
    , ( "+0"            , IntegerId 0           )
    , ( "-0"            , IntegerId 0           )
    , ( "123"           , IntegerId 123         )
    , ( "-123"          , IntegerId (-123)      )
    ]

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

testParser :: (Eq a, Show a) => String -> Parser a -> [(String, a)] -> IO (Int,Int)
testParser name parser tests =
    help tests [] (0 :: Int) (0 :: Int)
  where
    help [] es np nf = do
        putStrLn $ name ++ ": " ++ show np ++ " passed, " ++ show nf ++ " failed"
        mapM_ (putStrLn . ("  "++)) (reverse es)
        unless (null es) (putStrLn "")
        return (np, nf)
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

allCaps :: String -> [String]
allCaps []     = [[]]
allCaps (c:cs) =
    concatMap (\t -> [l:t, u:t]) (allCaps cs)
  where
    l = toLower c
    u = toUpper c
