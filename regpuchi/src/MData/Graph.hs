

module MData.Graph where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC

import Data.Word

data Node = Node
  {
    
    n_label :: Word8 ,
    index :: Word8
    
  }  deriving (Show)


data Edge = Edge
  {
    
    e_label :: Word8,
    bridge :: (Node,Node)
    
  } deriving (Show)


data Graph = Gr
  {
    
    g_node :: [Node] ,
    g_edge :: [Edge]
    
  } deriving (Show)


data AdjMatrix = AdjMatrix BS.ByteString deriving (Show)



