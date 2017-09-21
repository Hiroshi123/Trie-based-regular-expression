{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE KindSignatures #-}

module MData.Parser.ByteStr where

import MData.Parser.Base
import qualified Data.ByteString as BS --(ByteString,unpack)

-- import qualified Data.ByteString.Char8 as BC --()
--import Data.Char

import Data.Word
import MData.M

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
               

