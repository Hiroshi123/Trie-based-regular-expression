
module Parser_.Awk_.Action_.Statement_.Expression_.Base where


import Control_.M
import Parser_.Base_.Base
import Parser_.Base_.Bool
import Parser_.Base_.ByteStr
import Parser_.Base_.List
import Parser_.Base_.Int

-- import Parser_.Awk_.Action_.Expression_.Base
-- import Parser_.Awk_.Action_.Statement_.Assign
-- import Parser_.Awk_.Action_.Statement_.Print
-- import Parser_.Awk_.Pattern


import Parser_.Awk_.Data_.Data


-- + − * / % ˆ (exponentiation), and concatenation (indicated by white space).
-- The operators ! ++ −− += −= *= /= %= ˆ= > >= < <= == != ?: are also available in expressions.

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC

type Bs = BS.ByteString


assign_ :: Parser Expr
assign_ =
  (<->) **> anyLetters >==
  (\v -> with_space ( char "=" ) **> expr_ >== (\x -> r' (AssignExpr (v,x)) )
  )
  
--ope = 
  


expr_ :: Parser Expr
expr_ = assign_ <|> findex_ <|> str_ <|> int_

int_ :: Parser Expr
int_ = num >== (\x -> r' (Int' x) )

str_ :: Parser Expr
str_ = with_space str >== (\x -> r' (Str' x))
  
findex_ :: Parser Expr
findex_ =
  -- sepby1
  (
    (<->) **> char "$" **> num >== (\x -> r' (FieldIndex' x))
  )


