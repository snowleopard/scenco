module Tuura.Graph (Graph, GraphsFile, graphFilepath, loadGraph) where

data Graph
newtype GraphsFile = GraphsFile FilePath

graphFilepath :: GraphsFile -> FilePath
graphFilepath (GraphsFile file) = file

loadGraph :: FilePath -> GraphsFile
loadGraph = GraphsFile
