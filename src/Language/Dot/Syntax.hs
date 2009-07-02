-- | DOT AST. See <http://www.graphviz.org/doc/info/lang.html>.

module Language.Dot.Syntax where

data Graph
  = Graph GraphStrictness GraphDirectedness (Maybe Id) [Statement]
  deriving (Eq, Show)

data GraphStrictness
  = StrictGraph
  | UnstrictGraph
  deriving (Eq, Show)

data GraphDirectedness
  = DirectedGraph
  | UndirectedGraph
  deriving (Eq, Show)

data Id
  = NameId    String
  | StringId  String
  | IntegerId Integer
  | FloatId   Float
  | XmlId     Xml
  deriving (Eq, Show)

data Statement
  = NodeStatement       NodeId [Attribute]
  | EdgeStatement       [Entity] [Attribute]
  | AttributeStatement  AttributeStatementType [Attribute]
  | AssignmentStatement Id Id
  | SubgraphStatement   Subgraph
  deriving (Eq, Show)

data AttributeStatementType
  = GraphAttributeStatement
  | NodeAttributeStatement
  | EdgeAttributeStatement
  deriving (Eq, Show)

data Attribute
  = AttributeSetTrue  Id
  | AttributeSetValue Id Id
  deriving (Eq, Show)

data NodeId
  = NodeId Id (Maybe Port)
  deriving (Eq, Show)

data Port
  = PortI Id (Maybe Compass)
  | PortC Compass
  deriving (Eq, Show)

data Compass
  = CompassN  | CompassE  | CompassS  | CompassW
  | CompassNE | CompassNW | CompassSE | CompassSW
  deriving (Eq, Show)

data Subgraph
  = NewSubgraph (Maybe Id) [Statement]
  | SubgraphRef Id
  deriving (Eq, Show)

data Entity
  = ENodeId   EdgeType NodeId
  | ESubgraph EdgeType Subgraph
  deriving (Eq, Show)

data EdgeType
  = NoEdge
  | DirectedEdge
  | UndirectedEdge
  deriving (Eq, Show)

data Xml
  = XmlEmptyTag XmlName [XmlAttribute]
  | XmlTag      XmlName [XmlAttribute] [Xml]
  | XmlText     String
  deriving (Eq, Show)

data XmlName
  = XmlName String
  deriving (Eq, Show)

data XmlAttribute
  = XmlAttribute XmlName XmlAttributeValue
  deriving (Eq, Show)

data XmlAttributeValue
  = XmlAttributeValue String
  deriving (Eq, Show)
