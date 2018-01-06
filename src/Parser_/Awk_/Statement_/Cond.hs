

module Parser_.Awk_.Statement_.Cond where

import Control_.M
import Parser_.Base_.Base
import Parser_.Base_.Bool
import Parser_.Base_.ByteStr
import Parser_.Base_.List
import Parser_.Base_.Int

import Parser_.Awk_.Data_.Data
import Parser_.Awk_.Statement_.Expression_.Base
import Parser_.Awk_.Statement_.Assign

if_ :: Parser (Expr, [State_ -> State_])
if_ =
  
  with_space (string "if")
  **> between a compares c >==
  (\x -> assignSt >== (\y -> r' (x,y)) )
  where a = char "("
        c = char ")"
        
        
expr3 =
  -- assignment
  -- less or equal less
  -- increment
  expr_ >==
  (\x1 -> char ";" **> expr_ >==
    (\x2 -> char ";" **> expr_ >== (\x3 -> r' (x1,x2,x3)))
  )
  
for_ =
  with_space (string "for")
  **> between a expr3 c >==
  (\x -> assignSt >== (\y -> r' (x,y)) )
  where a = char "("
        c = char ")"
        
        
        
