

module Parser_.Base_.ByteStr where

import Parser_.Base_.Base
import Parser_.Base_.Bool


import qualified Data.ByteString as BS --(ByteString,unpack)
import qualified Data.ByteString.Char8 as BC --()
--import Data.Char

import Data.Word
import Control_.M

--type Bs = BS.ByteString

instance ParserC BS.ByteString where
  
  -- Conjunction (bytestring version)
  
  a <**> b =
    a >== 
    (\x -> b >== 
           (\y -> r' $ BS.append x y )
    )
    
    
  many x =  many1 x  <|> r' ""
  -- many1 is going to be ended when many1 returns empty list
  many1 p = p >== (\x -> many p >== (\y -> r' $ BS.append x y) )
  
  -- any =
  --   Parser $
  --   \x -> case x of
  --           ""  -> []
  --           _   -> [(BS.singleton $ BS.head x,BS.tail x)]
  
            
----------- Conjunction -------------------------------------------------------------------

  
---------------- following monadic functions are only for bytestring data type ---------------------

-- bottom parser which will produce bytestring Type

item :: Parser BS.ByteString
item =
  Parser $
  \x -> case x of
          ""  -> []
          _   -> [(BS.singleton $ BS.head x,BS.tail x)]
          
          
items :: Int -> Parser BS.ByteString
items a =
  Parser $
  \x -> case x of
          ""  -> []
          _   -> [(BS.take a x ,BS.drop a x)]
          
          
every  s = every1 s <|> ( end >== (\x -> r' s) )
every1 s = 
  (char s) >==
  (\_ -> (every s) >==
         (\_ -> (r' s))
  )
  
  
end :: Parser Bool -- BS.ByteString
end =
  Parser $
  \x -> case x of
          ""  -> [(True,"")]
          _   -> []
          
                 
          
          
satisfys' :: Int -> (BS.ByteString -> Bool) -> Parser BS.ByteString
satisfys' a f = items a >== (\x -> if f x then r' x else (<>))

satisfys :: BS.ByteString -> Parser BS.ByteString
satisfys bs =
  satisfys' l ( bs /= )
  where l = BS.length bs
  
-- many is going to be ended when many1 returns empty list
  
char :: BS.ByteString -> Parser BS.ByteString
char c = satisfy (c ==)

char_ :: String -> Parser BS.ByteString
char_ x =
  (<->) **>
  char (BC.pack x)
  <** (<->)
  
  
char__ :: BC.ByteString -> Parser BC.ByteString
char__ c =
  case (sCap c,lCap c) of
       (True, _ )   -> satisfy ( c == ) <|> satisfy ( a == )
         where a = BS.pack [(BS.head c) - 32]               
       ( _ , True ) -> satisfy ( c == ) <|> satisfy ( a == )
         where a = BS.pack [(BS.head c) + 32]
       otherwise    -> satisfy ( c == )
       
                       
string__ :: BC.ByteString -> Parser BS.ByteString
string__ "" = r' ""
string__ s = 
  (char__ h) >==
  (\_ -> (string__ t) >==
         (\_ -> (r' s))
  )
  where h = BS.singleton (BS.head s)
        t = BS.tail s
        
            
  -- case sCap x of
  --   True      -> satisfy ( lCap ( BS.pack [(BS.head x) - 32] ) || sCap x )
  --   otherwise -> satisfy ( sCap ( BS.pack [(BS.head x) + 32] ) || lCap x )
        
            
--char__ :: String -> Parser BS.ByteString
--char__ x = satisfy (c-32)
    
--cap_d_char = satisfy capLS

stringNot :: BS.ByteString -> Parser BS.ByteString
stringNot "" = r' ""
stringNot s = 
  ( satisfy (h /=) ) >==
  (\_ -> (stringNot t) >==
         (\_ -> (r' s))
  )
  where h = BS.singleton $ BS.head s
        t = BS.tail s
        

satisfy :: (BS.ByteString -> Bool) -> Parser BS.ByteString
satisfy f = item >== (\x -> if f x then r' x else (<>))

satisfyNot :: (BS.ByteString -> Bool) -> Parser BS.ByteString
satisfyNot f = item >== (\x -> if f x then (<>) else r' x )

string :: BS.ByteString -> Parser BS.ByteString
string "" = r' ""
string s = 
  (char h) >==
  (\_ -> (string t) >==
         (\_ -> (r' s))
  )
  where h = BS.singleton $ BS.head s
        t = BS.tail s
        
        
getALine =
  many (satisfys' 3 ( ( "bbb" :: BS.ByteString ) /= ))
  --many (satisfys ("b " :: BS.ByteString)) -- **< (items 2)
  
  
--alias-----------------------------------------------------

--(.>>) = many
--(>>.) = many1

------------------------------------------------------------
------ Useful functions for general parsing

quote :: Parser BS.ByteString
quote = squote <|> dquote

squote :: Parser BS.ByteString
squote = between a b a
  where a = char (BS.pack [39])
        b = anyLetters -- no many version
        
        
dquote :: Parser BS.ByteString
dquote = between a b a
  where a = char (BS.pack [34])
        b = many anyLetters
        
str = dquote
      
between a b c =  
  (<->) **> a **>  (<->) **>
  b **<  (<->) **< c **< (<->)
  
  
-- str :: Parser BS.ByteString
-- str =  
--   (<->) **>
--   char (BS.pack [34])
--   **>  (<->) **>
--   --many1 (satisfy letter)
--   many anyLetters
--   **<  (<->) **<
--   char (BS.pack [34])
--    **< (<->)


true_  = ( string "True"  <|> string "true"  ) >== (\_ -> r' "T")
false_ = ( string "False" <|> string "false" ) >== (\_ -> r' "F")

  
bool' :: Parser BS.ByteString
bool' = true_ <|> false_
  
  
bool :: Parser Bool
bool = bool' >==
  (\x ->
     case BS.head x of 
       84  -> r' True
       70  -> r' False
  )
  
  
--tab = satisfy (BC.pack "")

one_of x = satisfy (\a -> elem a x )

tab_sapce = many (one_of x)
  where x = fmap BC.pack [" ","\t"]
  
tab_sapce_sem = many (one_of x)
  where x = fmap BC.pack [" ","\t",";"]
  
            
skip_space :: Parser BS.ByteString
skip_space = many $ satisfy ((BC.pack " ") == )

(<->) =
  tab_sapce -- <|> commment (BC.pack "\\")
  
  --skip_space
  
  
(<-:>) =
  tab_sapce_sem
  
anyLetters =
  (<->) **>
  many1 (satisfy digitLetter_)
  <** (<->)
  
  
-- commment will end on \n
comment x =
  string x **> many item **> string "\n"
  
  
jump :: [Char] -> Parser BC.ByteString  
jump s =
  (<->) **>
  string (BC.pack s)
  <|> (<->)
  
  
comma :: Parser BS.ByteString
comma = satisfy ( BC.pack "," == )


with_space x = 
  (<->) **> x **< (<->)
  
-- blank_till_end =
--   (\x ->
--      case x of
--        [] -> 
       

--       )

  
