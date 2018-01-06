
module Parse where

import qualified Data.ByteString as BS --(ByteString,unpack)
import qualified Data.ByteString.Char8 as BC --(ByteString,unpack)

import Data.Word

-- myModule

import MData.M
import MData.Ps
import MData.RegE

--input' = BS.getLine >>= (\x -> return $ f11 (Start,x))

--conj :: Parser BC.ByteString
--conj = satisfy sCap

conj :: Parser RegE -> Parser RegE -> Parser RegE
conj a b =
  a >==
  (\x -> b >==
    (\y -> r' $ Conj x y)
  )
  
disj :: Parser RegE -> Parser RegE -> Parser RegE
disj a b =
  a >==
  (\x -> b >==
    (\y -> r' $ Disj x y)
  )
  
star :: Parser RegE -> Parser RegE
star a = a >== (\x -> r' $ Star x)

start :: Parser RegE
start = Parser $ \x -> [(Start,x)]

ruleConj :: Parser RegE -> Parser RegE
ruleConj a = a

-- ruleDisj :: Parser RegE -> Parser RegE
-- ruleDisj a = a <||>  

sepa = char (BC.pack "|")

ruleDisj :: Parser BS.ByteString
ruleDisj = 
  (char $ BC.pack "(") >==
  (\_ ->
     ruleDisj' >==
     (\y -> (char $ BC.pack ")") >==
       (\_ -> (r' y))
     )
  )

ruleDisj' :: Parser BS.ByteString
ruleDisj' = ruleConj start
            
-- ruleStar :: Parser BS.ByteString
-- ruleStar = 
--   (char $ BC.pack "(") >==
--   (\_ ->
--      many1 (satisfy sCap) >==
--      (\y -> (char $ BC.pack ")") >==
       
--        (\_ -> (char $ BC.pack "*") >==
--               (\_ -> (r' y))
--        )
--      )
--   )
