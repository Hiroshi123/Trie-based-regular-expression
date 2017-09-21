{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE KindSignatures #-}
 
module MData.Ps where

import qualified Data.ByteString as BS --(ByteString,unpack)
-- import qualified Data.ByteString.Char8 as BC --()

--import Data.Char
import Data.Word

-- myModule

import MData.M

data Parser a = Parser (BS.ByteString -> [(a,BS.ByteString)])


instance M Parser where
  -- return should cover Monad over its 1st argumennt.
  -- In this case, Both Parser data constructor and a function which is contained inside
  -- of Parser constructor is Monad.
  r' a = Parser $ \x -> [(a,x)]
  
  -- apply Bind whould be defined with the two steps
  -- First is to apply first class function inside first Monad,
  -- Second is to apply
  
  -- 1. m a1 -> (a1 -> m b) -> m b
  -- 2. m a2 -> (a2 -> m b) -> m b
  -- m a1 -> (a1 -> (m a2 -> (a2 -> m b) -> mb )) -> mb
  
  p >== f = Parser $ \x -> concat [ parse (f a) y | (a,y) <- parse p x ]
  
  
instance MPlus Parser where
  
  (<>) = Parser $ \_ -> []
  mmplus p q = Parser $ \x -> (parse p x) ++ (parse q x)

----------- Conjunction -------------------------------------------------------------------

-- Conjunction (list version)

(<**>) :: Parser [a] -> Parser [a] -> Parser [a]
a <**> b =
  a >== 
  (\x -> b >== 
         (\y -> r' $ x ++ y )
  )
  
-- Conjunction (bytestring version)

(<***>) :: Parser BS.ByteString -> Parser BS.ByteString -> Parser BS.ByteString
a <***> b =
  a >== 
  (\x -> b >== 
         (\y -> r' $ BS.append x y )
  )
  
----------- Disjunction -------------------------------------------------------------------
  
-- selection (every answers would be matched)
  
(<||>) :: Parser a -> Parser a -> Parser a
p <||> q = mmplus p q

-- selection (only leftest would be matched)

(<|>) :: Parser a -> Parser a -> Parser a
p <|> q
  = Parser
    $ \cs ->
        case parse (p `mmplus` q) cs of
          []     -> []
          (x:xs) -> [x]
          
-- lifting
  
parse :: Parser a -> BS.ByteString -> [(a,BS.ByteString)]
parse (Parser f) s = f s


---------------- following monadic functions are only for bytestring data type ---------------------

-- bottom parser which will produce bytestring Type

item :: Parser BS.ByteString
item =
  Parser $
  \x -> case x of
          ""  -> []
          _   -> [(BS.singleton $ BS.head x,BS.tail x)]
          
                 
satisfy :: (BS.ByteString -> Bool) -> Parser BS.ByteString
satisfy f = item >== (\x -> if f x then r' x else (<>))

char :: BS.ByteString -> Parser BS.ByteString
char c = satisfy (c ==)

string :: BS.ByteString -> Parser BS.ByteString
string "" = r' ""
string s = 
  (char h) >==
  (\_ -> (string t) >==
    (\_ -> (r' s))
  )
  where h = BS.singleton $ BS.head s
        t = BS.tail s
        
-- many is going to be ended when many1 returns empty list

many :: Parser BS.ByteString -> Parser BS.ByteString
many x =  many1 x  <|> r' ""

-- many1 is going to be ended when many1 returns empty list

many1 :: Parser BS.ByteString -> Parser BS.ByteString
many1 p = p >== (\x -> many p >== (\y -> r' $ BS.append x y) )

(.>>) = many
(>>.) = many1

-- following functions are to check if a given bytestring is in a certain range or not.
        
sCap :: BS.ByteString -> Bool
sCap x = (97 <= a && a <= 122)
         where a = BS.head x
               
                   
lCap :: BS.ByteString -> Bool
lCap x = (65 <= a && a <= 90)
         where a = BS.head x                   
               
letter :: BS.ByteString -> Bool               
letter x = sCap x || lCap x

digitLetter :: BS.ByteString -> Bool               
digitLetter x = digit x || letter x
                
digit :: BS.ByteString -> Bool
digit x = (48 <= a && a <= 57)
         where a = BS.head x
               
               
--input :: IO BS.ByteString                 
--input = BS.getLine >>= (\x -> return $ g1 gList x)



