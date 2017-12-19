
module Parser_.Base_.Bool where

--import Parser_.Base_.Base

import qualified Data.ByteString as BS --(ByteString,unpack)
import qualified Data.ByteString.Char8 as BC --()


-- functions to meet in ASC code range 

sCap :: BS.ByteString -> Bool
sCap x = (97 <= a && a <= 122)
         where a = BS.head x
               
                   
lCap :: BS.ByteString -> Bool
lCap x = (65 <= a && a <= 90)
         where a = BS.head x

underbar :: BS.ByteString -> Bool
underbar x = BS.head x == 95

dot :: BS.ByteString -> Bool
dot x = BS.head x == 46

haihun :: BS.ByteString -> Bool
haihun x = BS.head x == 45

colon :: BS.ByteString -> Bool
colon x = BS.head x == 58

semicolon :: BS.ByteString -> Bool
semicolon x = BS.head x == 59

equal :: BS.ByteString -> Bool
equal x = BS.head x == 61

letter :: BS.ByteString -> Bool  
letter x = sCap x || lCap x

letter_ :: BS.ByteString -> Bool     
letter_ x = sCap x || lCap x || underbar x || haihun x || dot x || colon x || semicolon x 

digitLetter :: BS.ByteString -> Bool               
digitLetter x = digit x || letter x

digitLetter_ :: BS.ByteString -> Bool            
digitLetter_ x = digit x || letter_ x
                 
digit :: BS.ByteString -> Bool
digit x = (48 <= a && a <= 57)
  where a = BS.head x
  
            
lineEnd :: BS.ByteString -> Bool
lineEnd x = a == 10
  where a = BS.head x


  
-- identifier [a-zA-Z_]+[0-9a-zA-Z_]*

         
         
  
             
----------------------------------------------------------------

