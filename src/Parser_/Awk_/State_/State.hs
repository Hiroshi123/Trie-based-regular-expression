

module Parser_.Awk_.State_.State where

import Control_.M
import Parser_.Base_.Base
import Parser_.Base_.Bool
import Parser_.Base_.ByteStr
import Parser_.Base_.List
import Parser_.Base_.Int

--import Parser_.Awk_.Action_.Expression_.Base
import Parser_.Awk_.Action_.Statement_.Assign
import Parser_.Awk_.Action_.Statement_.Print

import Parser_.Awk_.Pattern_.Pattern

import Parser_.Awk_.Data_.Data


import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC


  
init_state_ :: State_
init_state_ =
  State_
  {
    
    nr  = 0,
    nf  = 0,
    fs  = ',',
    ofs = 'a',
    ors = 'a',
    cur_text = "",
    cur_record = [],
    environ = [],
    output = ""
    
  }
  
get_record =
  (<->) **> sepby anyLetters (char " ") **< (<->)
  


update_state :: [(Pattern, Action)] -> State_ -> BC.ByteString -> State_
update_state f =
  
  (\(st@State_
      {
        nr = nr_ ,
        cur_text = text_ ,
        cur_record = pre_rec ,
        output = ot ,
        environ = env_ ,
        ..
      }
    ) a  ->
      let rec_ = fst $ head (parse get_record a)
          st_ = st
                {
                  cur_text   = a,
                  nr = nr_ + 1,
                  nf = length rec_,
                  cur_record = rec_,
                  output     = ot, --BS.append buf"\n"
                  environ    = env_                                 
                }                
      in update_state_ f st_          
  )
  
  
update_state_ :: [(Pattern, Action)] -> State_ -> State_
update_state_ [] st = st
update_state_ (h:t) st =
  case pf st of
    True  -> update_state_ t st_
      where st_ = foldl (\s1 f1 -> foldl (\s2 f2 -> f2 s2) s1 f1 ) st ac
    False -> update_state_ t st
  where (Pattern' pf , Action' ac) = h


top :: Parser [(Pattern, Action)]
top = sepby top_ (char "\n")

top_ :: Parser (Pattern, Action)
top_ =
  (
    (<->) **> pattern_ >==
    (\p -> (<->) **>
           in_brace action_ >== (\a -> r' (p,a))
    )
  )
  

in_brace :: Parser a -> Parser a
in_brace b = between a b c
  where a = char "{"
        c = char "}"


action_ :: Parser Action
action_ =
  (
    sepby
    (assignSt <|> print_ )
    (char ";")
  ) >== (\x -> r' (Action' x))
