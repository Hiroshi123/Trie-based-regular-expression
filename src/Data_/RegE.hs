
module Data_.RegE where

import Data.Word
import Data_.Graph

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC

type Bs = BS.ByteString

data RegE =
  Start
  | Epsilon'
  | Conj'    RegE RegE
  | Disj'    RegE RegE
  | Star'    RegE RegE
  | Plus'    RegE RegE
  | Hatena'  RegE RegE
  | Blace'   RegE RegE Int Int
  | Blacket' [RegE]
  | Dot'
  | C' Word8
  deriving (Show)

data Precedence =
  First | Second | Third
  deriving (Show,Enum)

op_signature :: [(RegE -> RegE -> RegE, Bs)]
op_signature = [(Star',"*"),(Conj',""),(Disj',"|")]

op_precedence :: [(RegE -> RegE -> RegE, Precedence)]
op_precedence = [(Star',First),(Conj',Second),(Disj',Third)]

data RegVM =
  Step Word8
  | Jump Word8
  | Split Word8 Word8
  | Match
  deriving (Show)

toGraph :: RegE -> Graph
toGraph Start = Gr [(Node 1 1)] []
toGraph (C' w) = Gr [(Node w w)] []

toGraph (Star' x _) = Gr ([Node 100 100] ++ node) edge
  where ret  = toGraph x
        node = g_node ret
        edge = g_edge ret
        
               
toGraph (Conj' l r) =
  Gr ([Node 99 99] ++ node) edge
  where lret = toGraph l
        rret = toGraph r
        node = (g_node lret) ++ (g_node rret)
        edge = (g_edge lret) ++ (g_edge lret)
        
          
toGraph (Disj' l r) =
  Gr ([Node 101 101] ++ node) edge
  where lret = toGraph l
        rret = toGraph r
        node = (g_node lret) ++ (g_node rret)
        edge = (g_edge lret) ++ (g_edge lret)
        
        
