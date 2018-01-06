
module Parser_.Base_.RegEP where


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

conj :: Parser RegE -> Parser RegE -> Parser RegE
conj a b =
  a >==
  (\x -> b >==
    (\y -> r' $ Conj x y)
  )
  
disj :: Parser RegE -> Parser RegE -> Parser RegE
disj a b =  
  a >== 
  (\x -> char "|" **> b >==
         (\y -> r' $ Disj x y)
  )
  
star :: Parser RegE -> Parser RegE
star a = a **< (char "*") >== (\x -> r' $ Star x)

         
ruleConj :: Parser RegE
ruleConj = rule__ `conj` rule_

ruleDisj :: Parser RegE
ruleDisj = rule___ `disj` rule_

expr_r = paren_ rule_ <|> paren_ symbol <|> symbol
expr_ = (ruleConj <|> ruleDisj <|> star rule__ <|> paren_ symbol) <|> expr_r -- paren_ rule_ <|> paren_ symbol <|> symbol


rule___ = paren_ rule_ <|> paren_ symbol <|> symbol
rule__ = paren_ rule_ <|> (rule___ `disj` rule_) <|> symbol
--rule   = paren_ rule_ <|> (rule__ `conj` rule_) <|> (rule___ `disj` rule_) <|> symbol
rule   = paren_ rule_ <|> (rule___ `disj` rule_) <|> star rule <|> symbol

-- rule_ :: Parser RegE

rule_ =
  (ruleConj <|> ruleDisj <|> star rule__ <|> paren_ symbol)
  
  
--rule_ = star rule <|> paren_ rule_  <|> ruleConj <|> ruleDisj <|> symbol


paren_ b = between a b c
  where a = char "("
        c = char ")"

star__ = star rule

--------------------------------------------------------------------


symbol = satisfy sCap >== (\x -> r' (C (BS.head x)))

