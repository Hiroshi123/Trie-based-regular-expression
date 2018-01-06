{-# LANGUAGE OverloadedStrings #-}

-- import Data.Graph.Inductive -- .Graph
-- import Data.GraphViz

-- import Data.Text.Lazy
-- import Data.GraphViz

import Data.Graph.Inductive.Example

-- import Data.GraphViz.Printing

import           Data.Graph.Inductive
import           Data.GraphViz
import           Data.GraphViz.Attributes.Complete
import           Data.GraphViz.Types.Generalised   as G
import           Data.GraphViz.Types.Monadic
import           Data.Text.Lazy                    as L
import           Data.Word

--import           WriteRunDot

main = putStrLn $ unpack $ renderDot $ toDot $ graphToDot nonClusteredParams a --clr479



