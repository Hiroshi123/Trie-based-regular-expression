


module Parser_.Awk_.Statement_.Expression.BuiltIn where

import Parser_.Awk_.Data_.Data

import Control_.M
import Parser_.Base_.Base
import Parser_.Base_.Bool
import Parser_.Base_.ByteStr
import Parser_.Base_.List
import Parser_.Base_.Int

--import Parser_.Awk_.Statement_.Assign
--import Parser_.Awk_.Statement_.Print
--import Parser_.Awk_.Statement_.Cond

-- expr__ = mem_access_

-- expr_ =
--   mem_access_ <|>
--   post_unary_ops_ <|>
--   pre_unary_ops_ <|>
--   compares <|>
--   binary_ops_ <|>
--   assigns
  
--  assign_ <|> findex_ <|> str_ <|> int_

-- math_ :: Parser Expr
-- math_ =
--   exp >== (\x -> r' x)


math_op_map_ :: [ ( Bs  , (Expr -> Expr) ) ]
math_op_map_ =
  [
    ("exp",Exp'),
    ("log",Log'),
    ("sqrt",Sqrt'),
    ("sin",Sin'),
    ("cos",Cos'),
    ("atan2",Atan2')
  ]
  
built_in_f_map_ :: [ ( Bs  , (Expr -> Expr) ) ]
built_in_f_map_ =
  [
    ("length",Length'),
    ("rand",Rand'),
    ("srand",SRand'),
    ("int",IntF'),
    ("substr",Substr'),
    ("index",IndexF'),
    ("match",Match'),
    ("split",Split'),
    ("sub",Subst'),
    ("gsub",Gsubst'),
    ("sprintf",Sprintf'),
    ("system",SystemF'),
    ("tolower",Tolower'),
    ("toupper",Toupper')
  ]



