{-# LANGUAGE CPP #-}
module Language.Dot.Pretty
  ( prettyPrintDot
  , renderDot
  , PP(..)
  )
  where

#if MIN_VERSION_base(4,11,0)
import Prelude hiding ((<>))
#endif

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

import Data.Foldable (toList)
import Numeric
import Text.PrettyPrint

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
  pp (Graph s d mi ss) = pp s <+> pp d <+> pp mi <+> lbrace $+$ indent (vcat' ss) $+$ rbrace

instance PP GraphStrictness where
  pp StrictGraph   = text "strict"
  pp UnstrictGraph = empty

instance PP GraphDirectedness where
  pp DirectedGraph   = text "digraph"
  pp UndirectedGraph = text "graph"

instance PP Id where
  pp (NameId v)    = text v
  pp (StringId v)  = doubleQuotes (text v)
  pp (IntegerId v) = integer v
  pp (FloatId v)   = ffloat v
  pp (XmlId v)     = langle <> pp v <> rangle

instance PP Statement where
  pp stmt = (<+> text ";") $ -- Always append ';'
    case stmt of
      (NodeStatement ni as)        ->
        pp ni
        <+> if not (null as) then brackets (hsep' as) else empty
      (AttributeStatement t as)    -> pp t <+> brackets (hsep' as)
      (AssignmentStatement i0 i1)  -> pp i0 <> equals <> pp i1
      (SubgraphStatement s)        -> pp s
      (EdgeStatement src tgts as)  ->
        (case src of
          ENodeId nid -> pp nid
          ESubgraph sub -> pp sub)
        <+> hsep' tgts
        <+> if not (null as) then brackets (hsep' as) else empty

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
  pp (NewSubgraph mi ss) = text "subgraph" <+> pp mi <+> lbrace $+$ indent (vcat' ss) $+$ rbrace
  pp (SubgraphRef i)     = text "subgraph" <+> pp i

instance PP Entity where
  pp (ENodeId ni)   = pp ni
  pp (ESubgraph sg) = pp sg

instance PP a => PP (WithEdge a) where
  pp (WithEdge et a) = pp et <+> pp a

instance PP EdgeType where
  pp DirectedEdge   = text "->"
  pp UndirectedEdge = text "--"

instance PP Xml where
  pp (XmlEmptyTag n as) = langle <> pp n <+> hsep' as <> slash <> rangle
  pp (XmlTag n as xs)   = langle <> pp n <+> hsep' as <> rangle <> hcat' xs <> langle <> slash <> pp n <> rangle
  pp (XmlText t)        = text t

instance PP XmlName where
  pp (XmlName n) = text n

instance PP XmlAttribute where
  pp (XmlAttribute n v) = pp n <> equals <> pp v

instance PP XmlAttributeValue where
  pp (XmlAttributeValue v) = doubleQuotes (text v)

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

indent :: Doc -> Doc
indent = nest 2

hcat' :: (Foldable f, PP a) => f a -> Doc
hcat' = hcat . map pp . toList

hsep' :: (Foldable f, PP a) => f a -> Doc
hsep' = hsep . map pp . toList

vcat' :: (Foldable f, PP a) => f a -> Doc
vcat' = vcat . map pp . toList

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

langle :: Doc
rangle :: Doc
slash  :: Doc

langle = char '<'
rangle = char '>'
slash  = char '/'

ffloat :: Float -> Doc
ffloat v = text (showFFloat Nothing v "")
