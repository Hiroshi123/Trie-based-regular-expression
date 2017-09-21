
module Tools.Vis
  (
    graphToDot,
    dotW
  )
where


import System.IO
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import Data.Bits 

import Data.Word

import MData.Graph

graphToDot :: Graph -> [BC.ByteString]
graphToDot g =
  
  [BC.pack "digraph aaa_aaa"] ++
  [BC.pack "{"] ++
  (nodeGet g) ++
  (edgeGet g) ++
  [ BC.pack "}" ]
  
  
nodeGet :: Graph -> [BC.ByteString]
nodeGet g = map (BS.singleton .  n_label) $ g_node g

edgeGet :: Graph -> [BC.ByteString]
edgeGet g = map (\x -> BS.append (BS.append (fst x) (BC.pack "->")) (snd x) ) (edgeGet' g)

edgeGet' :: Graph -> [(BC.ByteString, BC.ByteString)]
edgeGet' g = map (\x -> ( BS.singleton $ n_label (fst x) , BS.singleton $ n_label (snd x) ) ) a
  where a = map bridge $ g_edge g
  
  
dotW :: FilePath -> [BS.ByteString] -> IO ()
dotW fn xs =
  
  openFile fn WriteMode >>=
  (\handle ->
      mapM_ (BC.hPutStrLn handle) xs
      >>= (\_ -> hClose handle)
  )
  

