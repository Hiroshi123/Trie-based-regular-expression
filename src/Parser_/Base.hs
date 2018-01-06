{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE KindSignatures #-}

module MData.Parser.Base where

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
  
  
-- lifting
  
parse :: Parser a -> BS.ByteString -> [(a,BS.ByteString)]
parse (Parser f) s = f s

  
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
