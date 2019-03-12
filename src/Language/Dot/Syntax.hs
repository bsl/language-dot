-- | DOT AST. See <http://www.graphviz.org/doc/info/lang.html>.

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

module Language.Dot.Syntax where

import Data.List.NonEmpty
import Data.Data (Data)
import Data.Typeable (Typeable)
import GHC.Generics (Generic)

data Graph
  = Graph GraphStrictness GraphDirectedness (Maybe Id) [Statement]
  deriving (Data, Eq, Generic, Ord, Show, Typeable)

data GraphStrictness
  = StrictGraph
  | UnstrictGraph
  deriving (Data, Eq, Generic, Ord, Show, Typeable)

data GraphDirectedness
  = DirectedGraph
  | UndirectedGraph
  deriving (Data, Eq, Generic, Ord, Show, Typeable)

data Id
  = NameId    String
  | StringId  String
  | IntegerId Integer
  | FloatId   Float
  | XmlId     Xml
  deriving (Data, Eq, Generic, Ord, Show, Typeable)

data Statement
  = NodeStatement       NodeId [Attribute]
  | EdgeStatement       Entity (NonEmpty Entity) [Attribute]
  | AttributeStatement  AttributeStatementType [Attribute]
  | AssignmentStatement Id Id
  | SubgraphStatement   Subgraph
  deriving (Data, Eq, Generic, Ord, Show, Typeable)

data AttributeStatementType
  = GraphAttributeStatement
  | NodeAttributeStatement
  | EdgeAttributeStatement
  deriving (Data, Eq, Generic, Ord, Show, Typeable)

data Attribute
  = AttributeSetTrue  Id
  | AttributeSetValue Id Id
  deriving (Data, Eq, Generic, Ord, Show, Typeable)

data NodeId
  = NodeId Id (Maybe Port)
  deriving (Data, Eq, Generic, Ord, Show, Typeable)

data Port
  = PortI Id (Maybe Compass)
  | PortC Compass
  deriving (Data, Eq, Generic, Ord, Show, Typeable)

data Compass
  = CompassN  | CompassE  | CompassS  | CompassW
  | CompassNE | CompassNW | CompassSE | CompassSW
  deriving (Data, Eq, Generic, Ord, Show, Typeable)

data Subgraph
  = NewSubgraph (Maybe Id) [Statement]
  | SubgraphRef Id
  deriving (Data, Eq, Generic, Ord, Show, Typeable)

data Entity
  = ENodeId   EdgeType NodeId
  | ESubgraph EdgeType Subgraph
  deriving (Data, Eq, Generic, Ord, Show, Typeable)

data EdgeType
  = NoEdge
  | DirectedEdge
  | UndirectedEdge
  deriving (Data, Eq, Generic, Ord, Show, Typeable)

data Xml
  = XmlEmptyTag XmlName [XmlAttribute]
  | XmlTag      XmlName [XmlAttribute] [Xml]
  | XmlText     String
  deriving (Data, Eq, Generic, Ord, Show, Typeable)

data XmlName
  = XmlName String
  deriving (Data, Eq, Generic, Ord, Show, Typeable)

data XmlAttribute
  = XmlAttribute XmlName XmlAttributeValue
  deriving (Data, Eq, Generic, Ord, Show, Typeable)

data XmlAttributeValue
  = XmlAttributeValue String
  deriving (Data, Eq, Generic, Ord, Show, Typeable)
