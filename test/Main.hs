{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main (main) where

import           Control.Monad (unless)
import           Data.Char (toLower, toUpper)
import           Data.List.NonEmpty (NonEmpty((:|)))

import           Text.Parsec
import           Text.Parsec.String

import           Language.Dot.Parser
import           Language.Dot.Pretty
import           Language.Dot.Syntax

import qualified Generic.Random as R
import qualified Test.SmallCheck as S
import qualified Test.SmallCheck.Series as Series
import qualified Test.Tasty as T
import           Test.Tasty.HUnit ((@=?))
import qualified Test.Tasty.HUnit as H
-- import           Test.Tasty.QuickCheck ((===))
import qualified Test.Tasty.QuickCheck as Q
import qualified Test.Tasty.SmallCheck as S

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

main :: IO ()
main = do
    testParser "parsePort"      parsePort      parsePortTests
    testParser "parseCompass"   parseCompass   parseCompassTests
    testParser "parseAttribute" parseAttribute parseAttributeTests
    testParser "parseId"        parseId        parseIdTests
    T.defaultMain tastyTests

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

allCaps :: String -> [String]
allCaps []     = [[]]
allCaps (c:cs) =
    concatMap (\t -> [l:t, u:t]) (allCaps cs)
  where
    l = toLower c
    u = toUpper c

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

-- Tasty tests

nonEmptyNonQuote :: String -> String
nonEmptyNonQuote = ('x':) . filter (`notElem` ['"', ' ', '\n'])

instance Q.Arbitrary a => Q.Arbitrary (NonEmpty a) where
  arbitrary = (:|) <$> Q.arbitrary <*> Q.arbitrary

instance Q.Arbitrary a => Q.Arbitrary (WithEdge a) where
  arbitrary = R.genericArbitraryU

instance Q.Arbitrary XmlName where
  arbitrary = XmlName . nonEmptyNonQuote <$> Q.arbitrary

instance Q.Arbitrary Id where
  arbitrary = StringId . nonEmptyNonQuote <$> Q.arbitrary

instance Q.Arbitrary XmlAttributeValue      where arbitrary = R.genericArbitraryU
instance Q.Arbitrary XmlAttribute           where arbitrary = R.genericArbitraryU
instance Q.Arbitrary Xml                    where arbitrary = R.genericArbitraryU
instance Q.Arbitrary EdgeType               where arbitrary = R.genericArbitraryU
instance Q.Arbitrary Entity                 where arbitrary = R.genericArbitraryU
instance Q.Arbitrary Subgraph               where arbitrary = R.genericArbitraryU
instance Q.Arbitrary Compass                where arbitrary = R.genericArbitraryU
instance Q.Arbitrary Port                   where arbitrary = R.genericArbitraryU
instance Q.Arbitrary NodeId                 where arbitrary = R.genericArbitraryU
instance Q.Arbitrary Attribute              where arbitrary = R.genericArbitraryU
instance Q.Arbitrary AttributeStatementType where arbitrary = R.genericArbitraryU
instance Q.Arbitrary Statement              where arbitrary = R.genericArbitraryU
instance Q.Arbitrary GraphDirectedness      where arbitrary = R.genericArbitraryU
instance Q.Arbitrary GraphStrictness        where arbitrary = R.genericArbitraryU
instance Q.Arbitrary Graph                  where arbitrary = R.genericArbitraryU

instance Series.Serial m a => Series.Serial m (NonEmpty a)
instance Series.Serial m a => Series.Serial m (WithEdge a)

instance Monad m => Series.Serial m XmlName where
  series = Series.cons1 (XmlName . nonEmptyNonQuote)

instance Monad m => Series.Serial m Id where
  series = Series.cons1 (StringId . nonEmptyNonQuote)

instance Monad m => Series.Serial m XmlAttributeValue
instance Monad m => Series.Serial m XmlAttribute
instance Monad m => Series.Serial m Xml
instance Monad m => Series.Serial m EdgeType
instance Monad m => Series.Serial m Entity
instance Monad m => Series.Serial m Subgraph
instance Monad m => Series.Serial m Compass
instance Monad m => Series.Serial m Port
instance Monad m => Series.Serial m NodeId
instance Monad m => Series.Serial m Attribute
instance Monad m => Series.Serial m AttributeStatementType
instance Monad m => Series.Serial m Statement
instance Monad m => Series.Serial m GraphDirectedness
instance Monad m => Series.Serial m GraphStrictness
instance Monad m => Series.Serial m Graph

tastyTests :: T.TestTree
tastyTests = T.testGroup "QuickCheck tests"
  [
    -- Q.testProperty "Render then parse roundtrip" $ \g ->
    --   Right g === parseDot "test" (renderDot g)
    S.testProperty "Render then parse roundtrip" $ S.forAll $ \g ->
      Right g == parseDot "test" (renderDot g)
  , H.testCase "Empty graph" $
      Right (Graph StrictGraph UndirectedGraph Nothing [])
        @=? parseDot "test" "strict graph { }"
  , H.testCase "" $
      Right (Graph UnstrictGraph UndirectedGraph Nothing
             [EdgeStatement (ENodeId (NodeId (NameId "x") Nothing))
              (WithEdge UndirectedEdge (ENodeId (NodeId (NameId "y") Nothing)) :| []) []])
        @=? parseDot "test" "graph { x -- y; }"
  , H.testCase "Empty subgraph" $
      Right (Graph StrictGraph UndirectedGraph Nothing
             [SubgraphStatement (SubgraphRef (StringId "x.foo"))])
        @=? parseDot "test" "strict graph { subgraph \"x.foo\"; }"
  ]
