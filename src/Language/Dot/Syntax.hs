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
  deriving Show

data GraphStrictness
    = Strict
    | NotStrict
  deriving Show

data GraphDirectedness
    = Undirected
    | Directed
  deriving Show

data Id
    = NameId    String
    | StringId  String
    | IntegerId Integer
    | FloatId   Float
  deriving Show

data Statement
    = NodeStatement       NodeId [Attribute]
    | EdgeStatement       [Entity] [Attribute]
    | AttributeStatement  AttributeStatementType [Attribute]
    | AssignmentStatement Id Id
    | SubgraphStatement   Subgraph
  deriving Show

data AttributeStatementType =
      GraphAttributeStatement
    | NodeAttributeStatement
    | EdgeAttributeStatement
  deriving Show

data Attribute
    = AttributeSetTrue  Id
    | AttributeSetValue Id Id
  deriving Show

data NodeId = NodeId Id (Maybe Port)
  deriving Show

data Port
    = PortI Id (Maybe Compass)
    | PortC Compass
  deriving Show

data Compass
    = CompassN  | CompassE  | CompassS  | CompassW
    | CompassNE | CompassNW | CompassSE | CompassSW
  deriving Show

data Subgraph
    = NewSubgraph (Maybe Id) [Statement]
    | SubgraphRef Id
  deriving Show

data Entity
    = ENodeId   NodeId
    | ESubgraph Subgraph
  deriving Show
