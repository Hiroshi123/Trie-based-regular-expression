


module Parser_.Awk_.Action_.Statement_.Print where


import Control_.M
import Parser_.Base_.Base
import Parser_.Base_.Bool
import Parser_.Base_.ByteStr
import Parser_.Base_.List
import Parser_.Base_.Int

import Parser_.Awk_.Action_.Statement_.Expression_.Base

import Parser_.Awk_.Data_.Data

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC


-- import Parser_.Awk_.Action_.Statement_.Assign
-- import Parser_.Awk_.Action_.Statement_.Print
-- import Parser_.Awk_.Pattern


print_ :: Parser [( State_ -> State_)] -- [Statement]
print_ =
  with_space ( string "print" ) **> sepby expr_ (char ",")
  >== (\e -> r' (print_f e) )
  
--print_ = with_space ( string "print" ) **> expr_ >== (\e -> r' [ PRINT ( print_f_ e ) ] )

-- type RecordIndex = Int

(+++) = BS.append

print_f :: [Expr] -> [( State_ -> State_ )]

-- if there is no subsequent expression after print function , regard nothing as $0 = current text as itself
print_f [] =
  [
    (\st@State_ { cur_text = text_ , output = ot_ , .. } ->
       State_ {cur_text = text_ , output = (+++) ((+++) ot_ text_) "\n" , .. }
    )
  ]
  
print_f e  =
  print_f_ e ++
  [
    (\st@State_ { output = ot_ , .. } ->
       State_ { output = (+++) ot_ "\n" , .. }
    )
  ]
  
print_f_ :: [Expr] -> [( State_ -> State_ )]
print_f_ [] = []
print_f_ ((Str' a):t) =
  (\st@State_ { cur_record = rec_ , output = output_ , .. } ->
     State_ {cur_record = rec_ , output = (+++) output_ ((+++) " " a) , .. }
  ) : (print_f_ t)

print_f_ ((Int' a):t) =
  
  (\st@State_ { cur_record = rec_ , output = output_ , .. } ->
     State_ { cur_record = rec_ , output = BS.append output_ (BC.pack (show a)) , .. }
  ) : (print_f_ t)
  
  
print_f_ ((FieldIndex' i):t) =
  (\st@State_ { cur_text = text_ , cur_record = rec_ , output = ot_ , .. } ->
     case i of
       0 -> State_ { cur_text = text_ , cur_record = rec_ , output = (+++) ot_ text_ , .. }
       otherwise ->
         case getElem i rec_ of
           Just a  -> State_ { cur_text = text_ , cur_record = rec_ , output = (+++) ot_ a , .. }
           Nothing -> st
  
  ) : (print_f_ t)


getElem :: Int -> [a] -> Maybe a
getElem _ [] = Nothing
getElem i (h:t)
  | i == 1    = Just h
  | otherwise = getElem (i-1) t

