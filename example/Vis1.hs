
import System.IO
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import Data.Bits

import Data.Word
import MData.Graph
import Tools.Vis

node1 :: Node
node1 = Node 100 0
node2 = Node 98 1
node3 = Node 97 2

edge1 :: Edge
edge1 = Edge 97 (node1,node2)

edge2 :: Edge
edge2 = Edge 98 (node2,node1)
edge3 = Edge 99 (node1,node3)

graph1 :: Graph
graph1 = Gr [node1,node2,node3] [edge1,edge2,edge3]

draw = dotW "pika.dot" (graphToDot graph1)


