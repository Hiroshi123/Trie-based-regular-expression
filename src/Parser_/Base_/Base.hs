
module Parser_.Base_.Base where

import qualified Data.ByteString as BS --(ByteString,unpack)
import qualified Data.ByteString.Char8 as BC

--import Data.Char
import Data.Word

-- myModule

import Control_.M


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
  
  --p >== f = Parser $ \x -> [ parse (f a) y | (a,y) <- parse p x ]
  
  -- f1 >*> f2 = Parser
  --   $ \x -> case parse f1 x of
  --             [] -> []
  --             a  -> parse f2 state
  --               where state = snd (head a)

  -- left function would be firstly applied , and right function would be returned
  -- given updates of states provided by left function
  
  f1 **> f2 = Parser
    $ \x -> case parse f1 x of
              [] -> []
              a  -> parse f2 state
                where state = snd (head a)

  -- left function would be firstly applied.
  -- after looking out the result of right function, result of first (right) function
  -- would be returned.
  
  f1 **< f2 = Parser 
    $ \x -> case parse f1 x of
              [] -> []
              a  -> case parse f2 (snd (head a) ) of
                      [] -> []
                      b  ->  [ ( fst (head a) , snd (head b)) ]                      
                      
  -- right function would be firstly applied.
  --the value which would be returned would be second left function
  f2 <** f1 = Parser
    $ \x -> case parse f1 x of
              [] -> []
              a  -> parse f2 state
                where state = snd (head a)
                      
  -- right function would be applied.
  -- after
  
                        
  f2 >** f1 = Parser
    $ \x -> case parse f1 x of
              [] -> []
              a  -> case parse f2 (snd (head a)) of
                      [] -> []
                      b  ->  [ ( fst (head a) , snd (head b)) ]
                      
                      
                      
instance MPlus Parser where
  
  (<>) = Parser $ \_ -> []
  mmplus p q = Parser $ \x -> (parse p x) ++ (parse q x)
  
  
-- lifting

mplusP p q = Parser $ \x -> (parse p x) ++ (parse q (BS.tail x))

--p <|-> q = mplusP p q

p <|-> q
  = Parser
    $ \cs -> 
        case parse (p `mplusP` q) cs of
          []     -> []
          -- if its succeed, you will return soon. 
          (x:xs) -> [x]
          
          
parse :: Parser a -> BS.ByteString -> [(a,BS.ByteString)]
parse (Parser f) s = f s

--init :: Parser [a] 
init_ st = Parser $ \x -> [(st,x)]

-- getEval  :: [(a,BS.ByteString)] -> [a]
-- getEval s =
--   case parse f s of
--     [] -> 
--     [] -> 
                     
-- getState :: Parser a -> ByteString
-- getState (Parser f) s = f s
  
class ParserC a where
  --conjunction which  will let bind opeartion easy to be used. 
  (<**>) :: Parser a -> Parser a -> Parser a
  
  --disjunction which  will let bind opeartion easy to be used. 
  --(<||>) :: Parser a -> Parser a -> Parser a  
  
  -- star in regular expression which will provide arvitrary repeat of a given states,
  -- having a parser as an argument
  many  :: Parser a -> Parser a
  many1 :: Parser a -> Parser a
  
  --item  :: Parser a
  
  --satisfy :: (BS.ByteString -> Bool) -> Parser a
  
  -- char    :: BS.ByteString -> Parser a
  -- string  :: BS.ByteString -> Parser a
  
    
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
          
          
------------------------------------------------------------

               
------------------------------------------------------------------------------------

coverList :: Parser a -> Parser [a]
coverList x =
  x >== (\a -> r' [a] )
  
  

