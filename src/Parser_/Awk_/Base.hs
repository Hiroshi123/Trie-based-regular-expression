

module Parser_.Awk_.Base where

import Control_.M
import Parser_.Base_.Base
import Parser_.Base_.Bool
import Parser_.Base_.ByteStr
import Parser_.Base_.List
import Parser_.Base_.Int

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC

type Bs = BS.ByteString

data State_ = State_
  {
    
    --CONVFMT
    --convfmt :: [Char],
    
    nr  :: Int,
    nf  :: Int,
    --field seperator
    fs  :: Char,
    --output field separator
    ofs :: Char,
    --output record separator
    ors :: Char,
    -- ENVIRON
    
    environ :: VarList,
    cur_text ::  Bs,
    cur_record :: Record, -- [Bs],
    output :: Bs
    
  } deriving (Show)
  
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
  
  
type VarList = [(Bs,Expr)]

--sc_ps_ :: Parser [(State_ -> Bool, Statement)]
--sc_ps_ :: Parser [(State_ -> Bool, State_ -> State_)]


data PA_MAP =
  PA_MAP' [(Pattern,Action)]


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
  
  
--- **< (<->) **< char ";"

--- unlines "\n a  \n b \n fv "
  
-- line_break "\n ; "

in_brace :: Parser a -> Parser a
in_brace b = between a b c
  where a = char "{"
        c = char "}"
        
        
-- awk script is parsed before prog file is provided & converted to a list of map if is in the range of given grammer.
-- (key:pattern,value:action)


-- awk script is composed by sets of mapping of pattern & action.
-- if a pattern is matched to given linebuffer, action is executed to this line.
-- execution of each actions would be done when each lines are given.


-- pattern_ = string "BEGIN"
-- Variable names with special meanings:

data Pattern = Pattern' ( State_ -> Bool )

instance Show Pattern where
  show (Pattern' f ) = "ok"
    -- where (s,b) = f
    
pattern_ :: Parser Pattern
pattern_ = (begin_ <|> end_ <|> any_pattern_) >== (\x -> r' $ Pattern' x)

begin_ :: Parser (State_ -> Bool)
begin_ = string "BEGIN" >==
  
  (\_ -> r'
    (\st@State_ { cur_text = text_ , .. } -> nr == 0 )
  )
  
end_ :: Parser (State_ -> Bool)
end_ = string "END" >==
  (\_ -> r'
    (\st@State_ { cur_text = text_ , .. } -> nr == 0 )
  )
  
any_pattern_ :: Parser ( State_ -> Bool )
any_pattern_  = Parser (\x ->  [(\_ -> True,x)] )


---------------------------------------------------------------------

-- Action is a list of statements
--data Action = Action' [ [ (State_ -> State_) ] ] --[Statement]

--type Statement = [(State -> State)]

data Action = Action' [ [ (State_ -> State_) ] ] --[Statement]

  --deriving (Show)
  
instance Show Action where
  show (Action' s) =
    "number of action over rows is "   ++ show (length s) ++ "\n" ++
    "number of action over column is " ++ show (length (head s)) ++ "\n"
    
    
action_ :: Parser Action
action_ =
  (
    sepby
    (assignSt <|> print_ )
    (char ";")
  ) >== (\x -> r' (Action' x))
  
  
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

--print_f_ 
  
getElem :: Int -> [a] -> Maybe a
getElem _ [] = Nothing
getElem i (h:t)
  | i == 1    = Just h
  | otherwise = getElem (i-1) t
  
  
-- f2_ ((Str' a), = 
-- f2_ (Assign (Bs,Expr) ) = 
-- f2_ (Str' a) =  
-- f2_ (Int' Int) = 
-- f2_ (Float' Float)
-- f2_ (FieldIndex' Int)
    
  
-- data Expr =
--   F_Index' [Int]
--   deriving (Show)

  --anyLetters -- char "$" **> num -- >== (\x -> r' x)

-- action 

-- pattern returns first class function receiving states and returns bool value
-- action  returns first class function to update states

-- Statement is a data type which tells you how a provided states are updated.
-- Statements are terminated by semicolons, newlines or right braces.


data Statement = 
  Statement' [(State_ -> State_)]
  
  --   IF Expr Statement Statement
  -- | WHILE Expr Statement
  -- | PRINT (State_ -> State_)
  -- | PRINTF Expr


instance Show Statement where
  
  --show (PRINT a) = "ok!"
  show f = show "ok"
    
  
  --deriving (Show)

-- a_print1 =

--data Print =Print'' (State_ -> State_)


-- if( expression ) statement [ else statement ]
-- while( expression ) statement
-- for( expression ; expression ; expression ) statement
-- for( var in array ) statement
-- do statement while( expression )
-- break
-- continue
-- { [ statement ... ] }
-- expression # commonly var = expression print [ expression-list ] [ > expression ]
-- printf format [ , expression-list ] [ > expression ]
-- return [ expression ]
-- next
-- nextfile
-- delete array[ expression ]
-- delete array
-- exit [ expression ]


-- + − * / % ˆ (exponentiation), and concatenation (indicated by white space).
-- The operators ! ++ −− += −= *= /= %= ˆ= > >= < <= == != ?: are also available in expressions.

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
  
  --  ( char ",") >== (\a -> r' (FieldIndex' a) )

assignSt :: Parser [(State_ -> State_)]
assignSt =
  sepby assign_ (char ",") >==
  (\x -> r' (assign_f_ x))
  
assign_f_ [] = []
assign_f_ (h:t) = 
  (\st@State_ { environ = env_ , .. } ->
     case lookup (fst x) env_ of
       Just a  -> State_ { environ = env_, .. }
       Nothing -> State_ { environ = x : env_, .. }      
  ) : (assign_f_ t)
  where (AssignExpr x) = h
  
          
  
assign_ :: Parser Expr
assign_ =
  (<->) **> anyLetters >==
  (\v -> with_space ( char "=" ) **> expr_ >== (\x -> r' (AssignExpr (v,x)) )
  )
  
--ope = 
  
data Expr =
    AssignExpr (Bs,Expr)
  | Str' Bs
  | Int' Int
  | Float' Float
  | FieldIndex' Int
  
  -- | Singleton Char
  -- | Singleton Int
  -- | Singleton [Int]
  -- | Record' Record
  deriving (Show)

type Record = [Field]
type Field = Bs

-- data Record =
--   Field [Int]
--   deriving (Show)
