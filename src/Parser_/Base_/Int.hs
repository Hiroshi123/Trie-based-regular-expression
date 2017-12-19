
module Parser_.Base_.Int where

import Parser_.Base_.Base
import Parser_.Base_.Bool
import Parser_.Base_.ByteStr
import Parser_.Base_.List


import qualified Data.ByteString as BS --(ByteString,unpack)
import qualified Data.ByteString.Char8 as BC --()
--import Data.Char

import Data.Word
import Control_.M


instance ParserC Int where
  
  a <**> b =
    a >== 
    (\x -> b >== 
           (\y -> r' $ x * y )
    )
    
  many x =  many1 x  <|> r' 0
  
  -- many1 is going to be ended when many1 returns empty list
  
  many1 p = p >== (\x -> many p >== (\y -> r' $ toNum x y ) )
            where toNum x y = x * 10 + y
                  
                  
                  
  --many1 p = p >== (\x -> many p >== (\y -> r' $ foldl toNum 0 y) )
  --          where toNum x y = x * 10 + y
  
  --satisfy :: (BS.ByteString -> Bool) -> Parser BS.ByteString
  --satisfy f = item >== (\x -> if f x then r' 0 else r' 0)

            
            
itemN :: Parser Int
itemN = Parser
  $ \x -> case x of
            "" -> []
            _  -> [ ( fromIntegral $ BS.head x - 48, BS.tail x ) ]
            
            
num'' :: Parser Int
num'' = 
  (satisfy digit)
  >==
  (\x -> r' (toNum x))
  where toNum x = fromIntegral $ BS.head x - 48
  
  
--num' :: Parser [Int]
num' = coverList num'' <**> num' <|> r' []  -- (<>) -- []

--hei "" = 0
--hei x = foldl (\a y -> 10 * ( fromIntegral $ BS.head a - 48 ) + y ) 0 x

--hei :: Num t => BC.ByteString -> t

bsToNum :: BS.ByteString -> Int
bsToNum a = bsToNum' (BS.length a - 1)  a

bsToNum' :: Int -> BS.ByteString -> Int
bsToNum' d x =
  case d of
    0  -> bs1ToNum x
    _  -> (10 ^ d) * (bs1ToNum x) + b 
      where b = bsToNum' (d-1) (BS.tail x)
            
            
bs1ToNum :: BS.ByteString -> Int
bs1ToNum x = fromIntegral ( BS.head x - 48 )

             
num :: Parser Int
num =
  (<->) **>
  many1 ( satisfy (digit) ) >== (\x -> r' $ bsToNum x)
  **< (<->)
  
  
