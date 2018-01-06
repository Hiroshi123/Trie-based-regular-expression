


module Parser_.Base_.RegEP__ where

import qualified Data.ByteString as BS --(ByteString,unpack)
import qualified Data.ByteString.Char8 as BC --(ByteString,unpack)

import Data.Word

-- myModule

import Control_.M

import Parser_.Base_.Base
import Parser_.Base_.ByteStr
import Parser_.Base_.Bool

--import Data_.BNF.RegE

import Data_.RegE

expr   = disj_ <|> expr_
expr_  = conj_ <|> expr__
expr__ = star_ <|> plus_ <|> hatena_ <|> expr_t
expr_t = paren disj_ <|> paren conj_ <|> paren sig <|> sig -- <|> epsilon

paren b = between a b c
  where a = char "("
        c = char ")"
        
--terminal expression
sig = satisfy letter >== (\x -> r' (C' $ BS.head x))
epsilon = item0 >== (\_ -> r' Epsilon')

disj_   = op2_ expr_  (char "|",Disj') expr
conj_   = op2_ expr__ (item0   ,Conj') expr
-- post placed operator has epsilon as its second arugment
star_   = op2_ expr_t (char "*",Star') epsilon
plus_   = op2_ expr_t (char "+",Plus') epsilon
hatena_ = op2_ expr_t (char "?",Hatena') epsilon

op2_ ex1 (rec_,cons) ex2 =
  ex1 >==
  (\x1 ->
      rec_ **>
      ex2 >==
      (\x2 ->
          r' $ rotate (cons x1 x2)
      )
  )
  
rotate :: RegE -> RegE
rotate (Conj' a (Disj' b c)) = (Disj' (Conj' a b) c)
rotate (Star' a (Disj' b c)) = (Disj' (Star' a b) c)
rotate (Star' a (Conj' b c)) = (Conj' (Star' a b) c)
rotate a = a

to_psc :: RegE -> Parser Bs
to_psc (Conj' a b) = 
  to_psc a >== (\x1 -> to_psc b >== (\x2 -> r' (BS.append x1 x2)) )
to_psc (Disj' a b) =
  to_psc a <|> to_psc b
to_psc (Star' a _)   = many  (to_psc a)
to_psc (Plus' a _)   = many1 (to_psc a)
to_psc (Hatena' a _) = to_psc a <|> item0
to_psc (C' a) = char (BS.pack [a])


