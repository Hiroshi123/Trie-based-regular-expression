

import System.IO
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import Data.Bits 

import Data.Word

import MData.Graph

--import Data.ByteString.Base64 (decode)

bytestring = BC.pack "I'm a ByteString, not a [Char]"

dd :: [BS.ByteString]

dd = map BC.pack ["digraph aaa_aaa","{","a [shape=hexagon] ","b","c","a -> b","b -> a [label=f] ","c -> a","}"]

dd1 = map BC.pack ["digraph aaa_a","{","a [shape=hexagon] ","b","c","d","e","f","g","a -> b","a -> c","b -> d","b -> e","c -> f ","c -> g","}"]

dd2 = [BC.pack "digraph aaa_aaa"] ++ [BC.pack "{"] ++ f1 ++ f2' ++ [ BC.pack "}" ]
  
some :: [Word8]
some = [40,40,40]

divv :: Word8 -> [Word8] -> Word8
divv _ [] = 0 
divv w (h:t) =
  case mod w h  of
    0 -> h + ( divv w t )
    otherwise -> divv w t
    
    
aaa :: [Word8]
aaa = [1,2,4,8,16,32,64,128]



-- toDot :: Graph -> [String]
-- toDot 
  

--data Graph a = a

-- class MBits a where
--   (|||) :: a  -> a  -> a
--   (***) :: a  -> a  -> a

-- instance MBits Word8 where
--   (|||) :: a  -> a  -> a
--   (***) :: a  -> a  -> a
  



--AdjMatrix "()()"

--Right key = decode "kTSFoLQRrR+hWJlLjAwXqOH5Z3ZLDWray5mBgNK7lLuHdTwab8m/v96y"

--encrypt = BS.pack . BS.zipWith xor

--byteString

outputStrings :: FilePath -> [BS.ByteString] -> IO ()
outputStrings filename xs = 
  
  openFile filename WriteMode >>=
    (\handle ->
       mapM_ (BC.hPutStrLn handle) xs
       >>= (\_ -> hClose handle)
    )
    
    
