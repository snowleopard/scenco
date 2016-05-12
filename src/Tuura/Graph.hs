{-# LANGUAGE TypeFamilies, FlexibleContexts #-}
module Tuura.Graph (
    Graph (..), GraphFamily (..), GraphNormalForm, printGraph,
    GraphsFile, graphFilepath, loadGraph
    ) where

import Data.List.Ordered (union)

-- | An abstract class of graphs that can be overlayed and connected. Graph g
-- contains vertices of type @Vertex g@.
class Graph g where
    type Vertex g
    empty   :: g
    vertex  :: Vertex g -> g
    overlay :: g -> g -> g
    connect :: g -> g -> g

-- | A simple way to represent a graph is to list all its vertices and all its
-- arcs. We maintain the invariant that both lists are sorted.
data GraphNormalForm a = GraphNormalForm
    { vertices :: [a]
    , arcs     :: [(a, a)] }
  deriving (Eq, Show)

instance Ord a => Graph (GraphNormalForm a) where
    type Vertex (GraphNormalForm a) = a
    empty       = GraphNormalForm []  []
    vertex v    = GraphNormalForm [v] []
    overlay p q = GraphNormalForm (vertices p `union` vertices q)
                                  (arcs     p `union` arcs     q)
    connect p q = GraphNormalForm (vertices p `union` vertices q)
                                  (arcs     p `union` arcs     q `union`
                                  [ (u, v) | u <- vertices p, v <- vertices q ])

instance Functor GraphNormalForm where
    fmap f (GraphNormalForm vs as) = GraphNormalForm (fmap f vs) (fmap ff as)
      where
        ff (u, v) = (f u, f v)

-- | Graphs can be manipulated as if they were numbers. For example, (1 + 2) * 3
-- is a graph with three vertices 1, 2 and 3, and arcs (1, 3) and (2, 3).
-- Note, the negation is not very useful at the moment, but all Num laws are
-- satisfied. In future we may add support for more sensible negation
-- corresponding to graph complement.
instance (Ord a, Num a) => Num (GraphNormalForm a) where
    (+)         = overlay
    (*)         = connect
    abs         = id
    signum      = const empty
    fromInteger = vertex . fromInteger
    negate      = id

data GraphExpression = Open String | Closed String deriving (Eq, Ord)

close :: GraphExpression -> String
close (Open   s) = "(" ++ s ++ ")"
close (Closed s) = s

printGraph :: GraphExpression -> String
printGraph (Open   s) = s
printGraph (Closed s) = s

instance Graph GraphExpression where
    type Vertex GraphExpression = String
    empty       = Closed "()"
    vertex      = Closed . id
    overlay p q = Open $ printGraph p ++ " + " ++ printGraph q
    connect p q = Closed $ close p ++ " -> " ++ close q

-- | An abstract class for graph families that in addition to graphs have an
-- associated type of graph predicates.
class Graph g => GraphFamily g where
    type Predicate g
    condition :: Predicate g -> g -> g

newtype GraphsFile = GraphsFile FilePath

graphFilepath :: GraphsFile -> FilePath
graphFilepath (GraphsFile file) = file

loadGraph :: FilePath -> GraphsFile
loadGraph = GraphsFile
