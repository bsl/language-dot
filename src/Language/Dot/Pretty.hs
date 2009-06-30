module Language.Dot.Pretty
  (
    prettyPrintDot
  , renderDot
  )
  where

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

import Text.PrettyPrint (Doc, empty, render)
import Text.PrettyPrint ((<>), (<+>), ($+$))
import Text.PrettyPrint (hsep, nest, text, vcat)
import Text.PrettyPrint (brackets, colon, equals, lbrace, rbrace)
import Text.PrettyPrint (float, integer)
import qualified Text.PrettyPrint as TPP

import Language.Dot.Syntax

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

prettyPrintDot :: Graph -> Doc
prettyPrintDot = pp

renderDot :: Graph -> String
renderDot = render . pp

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

class PP a where
  pp :: a -> Doc

instance (PP a) => PP (Maybe a) where
  pp (Just v) = pp v
  pp Nothing  = empty

instance PP Graph where
  pp (Graph s d mi ss) = pp s <+> pp d <+> pp mi <+> lbrace $+$ indent (vcat (map pp ss)) $+$ rbrace

instance PP GraphStrictness where
  pp StrictGraph   = text "strict"
  pp UnstrictGraph = empty

instance PP GraphDirectedness where
  pp DirectedGraph   = text "digraph"
  pp UndirectedGraph = text "graph"

instance PP Id where
  pp (NameId v)    = text v
  pp (StringId v)  = text (show v)
  pp (IntegerId v) = integer v
  pp (FloatId v)   = float v

instance PP Statement where
  pp (NodeStatement ni as)       = pp ni <+> if not (null as) then brackets (hsep (map pp as)) else empty
  pp (EdgeStatement es as)       = hsep (map pp es) <+> if not (null as) then brackets (hsep (map pp as)) else empty
  pp (AttributeStatement t as)   = pp t <+> brackets (hsep (map pp as))
  pp (AssignmentStatement i0 i1) = pp i0 <> equals <> pp i1
  pp (SubgraphStatement s)       = pp s

instance PP AttributeStatementType where
  pp GraphAttributeStatement = text "graph"
  pp NodeAttributeStatement  = text "node"
  pp EdgeAttributeStatement  = text "edge"

instance PP Attribute where
  pp (AttributeSetTrue i)      = pp i
  pp (AttributeSetValue i0 i1) = pp i0 <> equals <> pp i1

instance PP NodeId where
  pp (NodeId i mp) = pp i <> pp mp

instance PP Port where
  pp (PortI i mc) = colon <> pp i <> maybe empty ((colon <>) . pp) mc
  pp (PortC c)    = colon <> pp c

instance PP Compass where
  pp CompassN  = text "n"
  pp CompassE  = text "e"
  pp CompassS  = text "s"
  pp CompassW  = text "w"
  pp CompassNE = text "ne"
  pp CompassNW = text "nw"
  pp CompassSE = text "se"
  pp CompassSW = text "sw"

instance PP Subgraph where
  pp (NewSubgraph mi ss) = text "subgraph" <+> pp mi <+> lbrace $+$ indent (vcat (map pp ss)) $+$ rbrace
  pp (SubgraphRef i)     = text "subgraph" <+> pp i

instance PP Entity where
  pp (ENodeId et ni)   = pp et <+> pp ni
  pp (ESubgraph et sg) = pp et <+> pp sg

instance PP EdgeType where
  pp NoEdge         = empty
  pp DirectedEdge   = text " ->"
  pp UndirectedEdge = text " --"

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

indent :: Doc -> Doc
indent = nest 2
