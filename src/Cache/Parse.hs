
module Parse where

import qualified Data.ByteString as BS --(ByteString,unpack)
import qualified Data.ByteString.Char8 as BC --(ByteString,unpack)

import Data.Word

-- myModule

import MData.M
import MData.Ps
import MData.RegE

input' = BS.getLine >>= (\x -> return $ f11 (Start,x))

conj :: Parser BC.ByteString
conj = satisfy sCap

f11 :: ( RegE , BS.ByteString ) -> [ RegE ]
f11 x
  | (snd x) == BS.empty = [fst x]
  
f11 x =
  case a of
    [] -> []
    otherwise -> f11 (head a)
  where a = f1 x [ (conj,Conj),(factorD,Disj),(factorS,Star) ]
  
  
data Fix f = Fix (f (Fix f)) --deriving (Show)

--fix = Fix (Star (Fix (Start)))

f1 :: ( RegE , BS.ByteString ) -> [ (Parser BS.ByteString,RegE->RegE->RegE) ] -> [ ( RegE , BS.ByteString ) ]
f1 _ [] = []
f1 x (h:t) =
  case a of
    []        -> f1 x t
    --otherwise -> [(Conj pre_res ( C $ BS.head res), rem)]
    otherwise -> [((snd h) pre_res res,rem)] --[(Conj pre_res ( C $ BS.head res), rem)]
      where res = C $ BS.head $ fst (head a)
            rem = snd (head a)            
  where str = snd x
        pre_res = fst x
        a = parse (fst h) str
        
        
sepa = char (BC.pack "|")

factorD :: Parser BS.ByteString
factorD = 
  (char $ BC.pack "(") >==
  (\_ ->
     disj1 >==
     (\y -> (char $ BC.pack ")") >==
       (\_ -> (r' y))
     )
  )
  
factorS :: Parser BS.ByteString
factorS = 
  (char $ BC.pack "(") >==
  (\_ ->
     many1 (satisfy sCap) >==
     (\y -> (char $ BC.pack ")") >==
       
       (\_ -> (char $ BC.pack "*") >==
              (\_ -> (r' y))
       )
     )
  )
  
-- disj1 :: Parser BS.ByteString
-- disj1 = (<||>) ( (<**>) disj1  disj1 ) ( many1 $ satisfy sCap )

disj1 :: Parser BS.ByteString
disj1 =  conj <***> sepa <***> conj -- many1 ( satisfy sCap )


--disj1 =  many1 (satisfy sCap) <***> sepa <***> many1 ( satisfy sCap )
--disj1 =  ( disj1 <***> sepa <***> disj1 ) <||> many1 ( satisfy sCap )



