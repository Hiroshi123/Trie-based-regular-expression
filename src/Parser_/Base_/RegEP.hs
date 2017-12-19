
module Parser_.Base_.RegEP where

import qualified Data.ByteString as BS --(ByteString,unpack)
import qualified Data.ByteString.Char8 as BC --(ByteString,unpack)

import Data.Word

-- myModule


import Control_.M

import Parser_.Base.Base
import Parser_.Base.ByteStr

import Data_.BNF.RegE



--input' = BS.getLine >>= (\x -> return $ f11 (Start,x))

--conj :: Parser BC.ByteString
--conj = satisfy sCap

-- Covering data Constructor to form Regular Expression Data Type

conj :: Parser RegE -> Parser RegE -> Parser RegE
conj a b =
  a >==
  (\x -> b >==
    (\y -> r' $ Conj x y)
  )
  
disj :: Parser RegE -> Parser RegE -> Parser RegE
disj a b =
  a >== 
  (\x -> sepa >== 
     (\_ -> b >== 
       (\y -> r' $ Disj x y)
     )
  )
  
star :: Parser RegE -> Parser RegE
star a = a >== (\x -> r' $ Star x)

---------------------------------------------------------------------

start :: Parser RegE
start = Parser $ \x -> [(Start,x)]

                       --- symbol
--- ruleConj --- rule1 --- ruleDisj
                       --- ruleStar
                       
                       
ruleConj :: Parser RegE -> Parser RegE
ruleConj s = s `conj` rule1

--ruleConj' :: Parser RegE 
--ruleConj' = rule1 <|> 
  
rule1 :: Parser RegE
rule1 = symbol <|> ruleDisj <|> ruleStar -- <||> []

--------------------------------------------------------------------

symbol :: Parser RegE
symbol =
  Parser
  $ \x ->
      let v = BS.head x
      in case (96 < v) && (v < 98) of
           True  -> [(C v,BS.tail x)]
           False -> []
      

sepa = char (BC.pack "|")

ruleDisj :: Parser RegE
ruleDisj = 
  (char $ BC.pack "(") >==
  (\_ ->
     ruleDisj' >==
     (\y -> (char $ BC.pack ")") >==
       (\_ -> (r' y))
     )
  )  
  
ruleDisj' :: Parser RegE
ruleDisj' = (ruleConj start) `disj` (ruleConj start)
            
ruleStar :: Parser RegE
ruleStar = 
  (char $ BC.pack "(") >==
  (\_ ->
     ruleConj start >==
     (\y -> (char $ BC.pack ")") >==
       
       (\_ -> (char $ BC.pack "*") >==
              (\_ -> (r' y))
       )
     )
  )
  
  
--input :: IO BS.ByteString
--input = BS.getLine >>= (\x -> return $ parse symbol x)
--input = BS.getLine >>= (\x -> return $ parse rule1 x)

input'' = BS.getLine >>= (\x -> return $ parse (ruleConj start) x)


