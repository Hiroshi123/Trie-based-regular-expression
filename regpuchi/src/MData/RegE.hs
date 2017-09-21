
module MData.RegE where

import Data.Word
import MData.Graph

data RegE =
  Start
  | Epsilon
  | Conj RegE RegE
  | Disj RegE RegE
  | Star RegE
  | C Word8
  deriving (Show)

data RegVM =
  Step Word8
  | Jump Word8
  | Split Word8 Word8
  | Match
  deriving (Show)


toGraph :: RegE -> Graph
toGraph Start = Gr [(Node 1 1)] []
toGraph (C w) = Gr [(Node w w)] []

toGraph (Star x) = Gr ([Node 100 100] ++ node) edge
  where ret  = toGraph x
        node = g_node ret
        edge = g_edge ret
        
        
               
toGraph (Conj l r) =
  Gr ([Node 99 99] ++ node) edge
  where lret = toGraph l
        rret = toGraph r
        node = (g_node lret) ++ (g_node rret)
        edge = (g_edge lret) ++ (g_edge lret)
        
          
toGraph (Disj l r) =
  Gr ([Node 101 101] ++ node) edge
  where lret = toGraph l
        rret = toGraph r
        node = (g_node lret) ++ (g_node rret)
        edge = (g_edge lret) ++ (g_edge lret)
        
        
