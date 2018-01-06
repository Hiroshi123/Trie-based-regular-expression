

module Parser_.Awk_.Statement_.Expression_.Base where

import Control_.M
import Parser_.Base_.Base
import Parser_.Base_.Bool
import Parser_.Base_.ByteStr
import Parser_.Base_.List
import Parser_.Base_.Int

import Parser_.Awk_.Data_.Data

-- + − * / % ˆ (exponentiation), and concatenation (indicated by white space).
-- The operators ! ++ −− += −= *= /= %= ˆ= > >= < <= == != ?: are also available in expressions.

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC

-- operator preference order
-- top level expression.

expr_ :: Parser Expr
expr_ =
  compares <|>
  binary_ops_ <|>
  post_unary_ops_ <|>
  mem_access_  <|>
  pre_unary_ops_ <|>
  assigns <|>
  int_ <|>
  str_ <|>
  findex_
  
-- this operation has expression as left side of its children.
-- Left side operation have to be ,

expr_l :: Parser Expr
expr_l =
  
  post_unary_ops_ <|>
  compares <|>
  binary_ops_ -- <|>
  -- int_ <|>
  -- str_ <|>
  -- findex_
  
expr_r :: Parser Expr
expr_r =
  
  mem_access_  <|>
  pre_unary_ops_ <|>
  --assigns <|> this is because assign is least prioritized operation
  int_ <|>
  str_ <|>
  findex_
  
mem_access_ :: Parser Expr
mem_access_ =
  
  anyLetters >==
  (\x1 -> many1 (coverList $ between (char "[") num (char "]") ) >==
    (\x2 -> r' (ListIndex' x1 x2))
  )
  
-- binary operation
  
assigns :: Parser Expr
assigns = expr2_ assign_map_ anyLetters expr_

-- to prevent (infinite) left-revusion, left child that it has should not contain any operator that has an expression as left side.
-- that is why, expr_r is set as first argument of expr2_
-- on the other hand, right-child should contain any expression to capture nested structure.

compares :: Parser Expr
compares = expr2_ compare_map_ expr_r expr_

binary_ops_ :: Parser Expr
binary_ops_ = expr2_ binary_op_map_ expr_r expr_

-- bin :: Parser Expr
-- bin =
  
--   expr_r >==
--   (\l -> binary_ops__ >==
--     (\op -> expr_ >==
--       (\r -> r' $ op l r)
--     )
--   )
  
  
--pre >== (\x -> binary_op_map_ expr_ >== (\y -> r' (x,y)) )

-- unary operation
pre_unary_ops_ :: Parser Expr
pre_unary_ops_ = pre_expr1_ pre_unary_op_map_ expr_

-- post unary ops does not have operators which has left child as an expression
post_unary_ops_ :: Parser Expr
post_unary_ops_ = post_expr1_ post_unary_op_map_ expr_r

-- pre-expr
pre_expr1_ :: [(Bs, t1 -> Expr)] -> Parser t1 -> Parser Expr
pre_expr1_ [] _ = (<>)
pre_expr1_ ((h1,h2):t) c1 =
  (
    (
      with_space ( string h1 ) **> c1 >== 
      (\x -> r' (h2 x) )
    )
  ) <|> pre_expr1_ t c1
  
  
-- post-expr
post_expr1_ :: [(Bs, t1 -> Expr)] -> Parser t1 -> Parser Expr
post_expr1_ [] _ = (<>)
post_expr1_ ((h1,h2):t) c1 =
  (
    (
      c1 **< with_space ( string h1 ) >== 
      (\x -> r' (h2 x) )
    )
  ) <|> post_expr1_ t c1
  
  
expr2_ :: [(Bs, t1 -> t2 -> Expr)] -> Parser t1 -> Parser t2 -> Parser Expr
expr2_ [] _ _ = (<>)
expr2_ ((h1,h2):t) c1 c2 =
  (
    (<->) **> c1 >==
    (\v -> ( string h1 ) **< (<->)
      **> c2 >== (\x -> r' (h2 v x) )
    )
  ) <|> expr2_ t c1 c2
  
  
-- there are 4 different types of mapping of signature and Constructor.

-- mem_access_map_ :: [ ( Bs  , ( Bs -> Expr -> Expr) ) ]
-- mem_access_map_ =
--   [
--     ("",Inc')
--   ]

assign_map_ :: [ ( Bs  , ( Bs -> Expr -> Expr) ) ]
assign_map_ =
  [
    ("=" ,Assign'),
    ("+=",Inc'   ),
    ("-=",Dec'   ),
    ("*=",MulEq' ),
    ("/=",DivEq' ),
    ("%=",ModEq' ),
    ("^=",ExptEq')
  ]
  
compares_ :: [( Bs  , ( Expr -> Expr -> Expr ) )] -> Parser Expr
compares_ ((h1,h2):t) =
  (
    (<->) **> expr_ >==
    (\v -> with_space ( char h1 ) **> expr_ >== (\x -> r' (h2 v x) ))
  ) <|> compares_ t
  
  
compare_map_ :: [ ( Bs  , ( Expr -> Expr -> Expr ) ) ]
compare_map_ = 
  [
    (">",Greater'),
    (">=",GreaterEq'),
    ("<",Less'),
    ("<=",LessEq'),
    ("==",Equal'),
    ("!=",NotEq')
  ]
  
  
binary_op_map_ :: [ ( Bs  , ( Expr -> Expr -> Expr ) ) ]
binary_op_map_ = 
  [
    ("+",Plus'),
    ("-",Minus'),
    ("*",Mul'),
    ("/",Div'),
    ("^",Expt'),
    (" ",Concat')
  ]
  
pre_unary_op_map_ :: [ ( Bs  , ( Expr -> Expr ) ) ]
pre_unary_op_map_ = 
  [
    ("++",UnaryPrePlus'),
    ("--",UnaryPreMinus'),
    ("-" ,Negative')
    
  ]
  
post_unary_op_map_ :: [ ( Bs  , ( Expr -> Expr ) ) ]
post_unary_op_map_ =
  [
    ("++",UnaryPostPlus'),
    ("--",UnaryPostMinus')
  ]
  
-- terminal expression which represents a type

int_ :: Parser Expr
int_ = num >== (\x -> r' $ Load' (Int' x) )
--int_ = num >== (\x -> r' (Int' x) )

str_ :: Parser Expr
str_ = str >== (\x -> r' $ Load' (Str' x))

findex_ :: Parser Expr
findex_ =
  -- sepby1
  (
    (<->) **> char "$" **> num >== (\x -> r' $ Load' (FieldIndex' x))
  )
  
  
  
