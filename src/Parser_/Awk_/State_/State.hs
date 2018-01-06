

module Parser_.Awk_.State_.State where

import Control_.M
import Parser_.Base_.Base
import Parser_.Base_.Bool
import Parser_.Base_.ByteStr
import Parser_.Base_.List
import Parser_.Base_.Int

import Parser_.Awk_.Action_.Action

import Parser_.Awk_.Pattern_.Pattern

import Parser_.Awk_.Data_.Data

import Parser_.Awk_.Statement_.Expression_.Base

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC


-- a set of state which are set as default.
-- states are shared over lines and mutable (data structure is immutable)
-- 

init_state_ :: State_
init_state_ =
  State_
  {
    
    -- this is given when this state are initialized (before entering in a fist line)
    special_pattern = Just BEGIN,
    filename = "",
    argc = 0,
    argv = [],
    
    -- field seperator
    fs  = ',',
    -- record seperator
    rs  = ' ',
    
    -- output field seoerator
    ofs = ' ',
    -- output record seperator
    ors = '\n',

    -- index length  of current record
    nr  = 0,

    -- index length  of current record
    -- on this file ( if you feed mutiple files as input arguments,
    -- nr > fnr )
    fnr = 0,
    -- number of fields
    nf  = 0,
    
    -- current text
    cur_text = "",
    -- current record
    cur_record = [],

    -- when user defined variable is given,
    -- they are in this map
    environ = [],
    
    -- function_env = [],
    
    -- stdout output buffer
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
  
  
f :: ( State_ , Expr ) -> (State_ , Expr)
f (st,(Less' (Load' a) (Load' b))) =
  ( exec'' st ex_r , Load' ex_r )
  where ex_r = eval (Less' a b )
  
f (st,(Less' a (Load' b))) =
  ( exec'' st ex_r , (Load' ex_r) )
  where (st_ , ex_) = f (st,a)
        ex_r = eval (Less' ex_ b)
        
        
f (st,(Less' (Load' a) b)) =
  ( exec'' st ex_r , (Load' ex_r) )
  where (st_ , ex_) = f (st,b)
        ex_r = eval (Less' ex_ a)
        
        
f (st,(Less' a b)) =
  ( exec'' st ex_ , (Load' ex_) )
  where (st_l , ex_l) = f (st,a)
        (st_r , ex_r) = f (st,b)
        ex_ = eval (Less' ex_l ex_r)
        
        
exec' :: ( State_ , Expr ) -> Expr
exec' (st,(Load' (Int' a))) = Int' a

exec'' :: State_ -> Expr -> State_
exec'' a (Assign' _ _) = a
exec'' st@State_
  { cur_record = rec_, .. }
  (Print' _)
  = State_ { cur_record = rec_ , .. }
  


--  where st_ = {cur_record = "" , ..}
  
  -- (\st@State_ { cur_record = rec_ , output = output_ , .. } ->
  --    State_ {cur_record = rec_ , output = (+++) output_ ((+++) " " a) , .. }
  -- ) : (print_f_ t)

eval :: Expr -> Expr

eval (UnaryPrePlus'  (Int' a)) = Int' (a+1)
eval (UnaryPreMinus' (Int' a)) = Int' (a-1)
eval (Negative' (Int' a)) = Int' (-a)  
  
eval (Plus'    (Int' x1) (Int' x2)) = Int' (x1 + x2)
eval (Plus'    (Float' x1) (Float' x2)) = Float' (x1 + x2)

eval (Minus'   (Int' x1) (Int' x2)) = Int' (x1 - x2)
eval (Minus'   (Float' x1) (Float' x2)) = Float' (x1 - x2)

eval (Mul'     (Int' x1) (Int' x2)) = Int' (x1 * x2)
eval (Mul'     (Float' x1) (Float' x2)) = Float' (x1 * x2)

--eval (Div'     (Int' x1) (Int' x2)) = Int' (x1 / x2)

eval (Div'     (Float' x1) (Float' x2)) = Float' (x1 / x2)

eval (Concat' (Str' x1) (Str' x2)) = Str' (BS.append x1 x2)

eval (Less'    (Int' x1) (Int' x2)) = Bool' (x1 < x2)
eval (Equal'   (Int' x1) (Int' x2)) = Bool' (x1 == x2)
eval (Greater' (Int' x1) (Int' x2)) = Bool' (x1 > x2)


eval x = x
  where (Less' x1 x2) = x
        x1_ = eval x1
        x2_ = eval x2
        
              
  -- | ex == Greater' x1 x2 = True
  -- | otherwise            = False
  
  
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


