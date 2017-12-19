
import System_.IO

import System.IO
import System.Directory

import Parser_.Base_.Base 
import Parser_.Awk_.State_.State

import qualified Data.ByteString as BS --(ByteString,unpack)
import qualified Data.ByteString.Char8 as BC

-- _read fn = read_ fn pre_process

unlines_ x = foldr (\a b -> BS.append a b) "" x

read_ sc fn =  
  getCurrentDirectory >>= 
  (\cd ->
     withFile (cd ++ "/" ++ fn) ReadMode
     (\h -> hGetLines h >>=     
            (\x -> return ( foldl (update_state f) init_state_ x ) )
     )
  ) where f = fst $ head $ parse top sc
  
  
