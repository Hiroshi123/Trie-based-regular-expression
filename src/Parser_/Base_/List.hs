

module Parser_.Base_.List where

import Parser_.Base_.Base
import Parser_.Base_.Bool

import Parser_.Base_.ByteStr

import qualified Data.ByteString as BS --(ByteString,unpack)
import qualified Data.ByteString.Char8 as BC --()
--import Data.Char

import Data.Word
import Control_.M


--instance ParserC [BS.ByteString] where

instance ParserC [a] where
  
  a <**> b =
    a >== 
    (\x -> b >== 
           (\y -> r' $ x ++ y )
    )
    
  many x =  many1 x  <|> r' []
  many1 p = p >== (\x -> many p >== (\y -> r' $ x ++ y) )
  
  
--------------------------------------------

itemL :: Parser [BS.ByteString]
itemL = Parser $ \x ->
  case x of
    "" -> []
    _  -> [([BS.singleton $ BS.head x],BS.tail x)]
    

satisfyL f = itemL >== (\x -> if f (head x) then r' x else (<>))
    
charL :: BS.ByteString -> Parser [BS.ByteString]
charL c = satisfyL (c == )

stringL :: BS.ByteString -> Parser [BS.ByteString]
stringL (BS.uncons -> Nothing) = r' [BS.empty]
                                 
stringL s = (charL h) **> (stringL t) **> r' [s]
  where h = BS.singleton $ BS.head s
        t = BS.tail s


-- sepby :: Parser a -> Parser b -> Parser [a]
-- p `sepby` sep = r' -- (p `sepby1` sep) ++ r' []

--sepby1 :: Parser a -> Parser b -> Parser [a]

--sepby1 :: ParserC a1 => Parser a1 -> Parser a -> Parser [a1]
-- p `sepby1` sep =
--   p >==
--   (\a ->
--      many (fh sep p)  >==
--      (
--        \as -> r' (a:[as])
--      )
--   )

sepby f s = sepby1 f s <|> r' []
sepby1 f s = coverList f <**> many ( s **> coverList f )

--------------------------------------------

bList :: Parser [BS.ByteString]
bList = many1 $ coverList ( skip_space **> many1 ( satisfy letter ) )


