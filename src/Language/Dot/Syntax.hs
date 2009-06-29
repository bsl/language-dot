module Language.Dot.Syntax
  (
    Graph(..)
  , GraphStrictness(..)
  , GraphDirectedness(..)
  , Id(..)
  , Statement(..)
  , AttributeStatementType(..)
  , Attribute(..)
  , NodeId(..)
  , Port(..)
  , Compass(..)
  , Subgraph(..)
  , Entity(..)
  )
  where

data Graph
    = Graph GraphStrictness GraphDirectedness (Maybe Id) [Statement]
  deriving (Eq, Show)

data GraphStrictness
    = Strict
    | NotStrict
  deriving (Eq, Show)

data GraphDirectedness
    = Undirected
    | Directed
  deriving (Eq, Show)

data Id
    = NameId    String
    | StringId  String
    | IntegerId Integer
    | FloatId   Float
  deriving (Eq, Show)

data Statement
    = NodeStatement       NodeId [Attribute]
    | EdgeStatement       [Entity] [Attribute]
    | AttributeStatement  AttributeStatementType [Attribute]
    | AssignmentStatement Id Id
    | SubgraphStatement   Subgraph
  deriving (Eq, Show)

data AttributeStatementType =
      GraphAttributeStatement
    | NodeAttributeStatement
    | EdgeAttributeStatement
  deriving (Eq, Show)

data Attribute
    = AttributeSetTrue  Id
    | AttributeSetValue Id Id
  deriving (Eq, Show)

data NodeId = NodeId Id (Maybe Port)
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
    = ENodeId   NodeId
    | ESubgraph Subgraph
  deriving (Eq, Show)
