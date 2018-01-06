
module Parser_.Awk_.Statement_.Assign where


import Control_.M
import Parser_.Base_.Base
import Parser_.Base_.Bool
import Parser_.Base_.ByteStr
import Parser_.Base_.List
import Parser_.Base_.Int

import Parser_.Awk_.Statement_.Expression_.Base

-- import Parser_.Awk_.Action_.Statement_.Assign
-- import Parser_.Awk_.Action_.Statement_.Print
-- import Parser_.Awk_.Pattern

import Parser_.Awk_.Data_.Data

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC

assignSt :: Parser [(State_ -> State_)]
assignSt =
  sepby assigns (char ",") >==
  (\x -> r' (assign_f_ x))
  
assign_f_ [] = []
assign_f_ (h:t) = 
  (\st@State_ { environ = env_ , .. } ->
     case lookup x1 env_ of
       Just a  -> State_ { environ = env_, .. }
       Nothing -> State_ { environ = (x1,x2) : env_, .. }      
  ) : (assign_f_ t)
  where (Assign' x1 x2) = h
  
-- assign_ :: Parser Expr
-- assign_ =
--   (<->) **> anyLetters >==
--   (\v -> with_space ( char "=" ) **> expr_ >== (\x -> r' (AssignExpr (v,x)) )
--   )


