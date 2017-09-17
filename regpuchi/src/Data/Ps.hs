{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE KindSignatures #-}

module Data.Ps where

import qualified Data.ByteString as BS --(ByteString,unpack)
-- import qualified Data.ByteString.Char8 as BC --()

--import Data.Char
import Data.Word

-- myModule

import Data.M

--import Data.ByteString (ByteString)
--import Data.ByteString.Char8 ()

--import Control.Monad
--import Data.Char

data Parser a = Parser (BS.ByteString -> [(a,BS.ByteString)])

instance M Parser where
  -- return should cover Monad .
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
  
  
(<||>) :: Parser a -> Parser a -> Parser a
p <||> q = mmplus p q

(<|>) :: Parser a -> Parser a -> Parser a
p <|> q
  = Parser
    $ \cs ->
        case parse (p `mmplus` q) cs of
          []     -> []
          (x:xs) -> [x]
          
          
(<**>) :: Parser [a] -> Parser [a] -> Parser [a]
a <**> b =
  a >== 
  (\x -> b >== 
         (\y -> r' $ x++y )
  )
  
  
parse :: Parser a -> BS.ByteString -> [(a,BS.ByteString)]
parse (Parser f) s = f s

item :: Parser BS.ByteString
item =
  Parser $
  \x -> case x of
          ""  -> []
          _   -> [(BS.singleton $ BS.head x,BS.tail x)]
          
                 
satisfy :: (BS.ByteString -> Bool) -> Parser BS.ByteString
satisfy f = item >== (\x -> if f x then r' x else (<>))

-- many is going to be ended when many1 returns empty list
many :: Parser a -> Parser [a]
many x =  many1 x <|> r' []

-- many1 is going to be ended when many1 returns empty list
many1 :: Parser a -> Parser [a]
many1 p = p >== (\x -> many1 p >== (\y -> r' $ x:y) )

(.>>) = many
(>>.) = many1

f :: BS.ByteString -> Bool
f x = case BS.head x of 
        100       -> True
        otherwise -> False
        
        
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
                              

