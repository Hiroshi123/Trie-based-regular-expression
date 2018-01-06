

module Parser_.Awk_.Data_.Data where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC

type Bs = BS.ByteString

data SpecialPattern =
  BEGIN | END
  deriving (Enum,Show)

data State_ = State_
  {    
    --CONVFMT
    --convfmt :: [Char],
    special_pattern :: Maybe SpecialPattern,
    filename :: Bs,
    
    -------------------------------------
    -- separator
    
    --field seperator
    fs  :: Char,
    --record seperator
    rs  :: Char,
    
    --output field separator
    ofs :: Char,
    --output record separator
    ors :: Char,
    
    -- argument count
    argc :: Int,
    -- argument array
    argv :: [Bs],
    
    --ofmat  :: Bs e.g. %.6g
    --subsep :: 034
    
    -- following values are going to be fed line by line
    -- line number (incremented when having a new line)
    nr  :: Int,
    fnr :: Int,
    nf  :: Int,
    
    -- ENVIRON
    environ :: VarList,
    -- text of current input
    cur_text ::  Bs,
    -- record which was separated from a current line
    cur_record :: Record, -- [Bs],
    
    --
    
    output :: Bs
    
  } deriving (Show)

type VarList = [(Bs,Expr)]

-- awk script is parsed before prog file is provided & converted to a list of map if is in the range of given grammer.
-- (key:pattern,value:action)


-- awk script is composed by sets of mapping of pattern & action.
-- if a pattern is matched to given linebuffer, action is executed to this line.
-- execution of each actions would be done when each lines are given.


-- pattern_ = string "BEGIN"
-- Variable names with special meanings:

data Declaration =
  Declaration' ([Function],[(Pattern, Action)])
  deriving (Show)


data Function = Function' Fname [ [ (State_ -> State_) ] ] -- [Statement]
--  deriving (Show)

instance Show Function where
  show (Function' _ s) =
    "number of action over rows is "   ++ show (length s) ++ "\n" ++
    "number of action over column is " ++ show (length (head s)) ++ "\n"
    
    
type Fname = Bs

data Pattern = Pattern' ( State_ -> Bool )

instance Show Pattern where
  show (Pattern' f ) = "ok"
    -- where (s,b) = f

data Action = Action' [ [ (State_ -> State_) ] ] --[Statement]

  --deriving (Show)
  
instance Show Action where
  show (Action' s) =
    "number of action over rows is "   ++ show (length s) ++ "\n" ++
    "number of action over column is " ++ show (length (head s)) ++ "\n"
    
--
    
data Statement = 
  Statement' [(State_ -> State_)]
  
  --   IF Expr Statement Statement
  -- | WHILE Expr Statement
  -- | PRINT (State_ -> State_)
  -- | PRINTF Expr
  
  
instance Show Statement where
  
  --show (PRINT a) = "ok!"
  show f = show "ok"
  
data Instruction =
    STORE'
--  | STORE_STDOUT'
  | LOAD_CONST'
  | LOAD_NAME'
  | PLUS'  
  | MINUS' 
  | MUL'  
  | DIV'
  | GREATER'
  | EQ'
  | NOT'
  | JUMP'
  | CALL'
  deriving (Enum,Show)

-- nd are built using the operators + − * / % ˆ (exponentiation),
-- and concate- nation (indicated by white space).
-- The operators ! ++ −− += −= *= /= %= ˆ= > >= < <= == != ?:

data Expr =
  
  -- assign operation

  Print'         Expr
  
  | Assign' Bs   Expr
  | Inc'    Bs   Expr
  | Dec'    Bs   Expr
  | MulEq'  Bs   Expr
  | DivEq'  Bs   Expr
  | ModEq'  Bs   Expr
  | ExptEq' Bs   Expr
  
  -- compare
  | Greater'   Expr Expr
  | GreaterEq' Expr Expr
  | Less'      Expr Expr
  | LessEq'    Expr Expr
  | Equal'     Expr Expr
  | NotEq'     Expr Expr
  
  -- binary operator
  | Plus'   Expr Expr
  | Minus'  Expr Expr
  | Mul'    Expr Expr
  | Div'    Expr Expr 
  | Mod'    Expr Expr
  | Expt'   Expr Expr

  --this is just space 
  | Concat' Expr Expr
  -- | Concat' Expr Expr
  
  -- unary pre operator
  
  | UnaryPrePlus'  Expr
  | UnaryPreMinus' Expr
  | Negative'      Expr
  
  -- unary post operator
  | UnaryPostPlus'  Expr
  | UnaryPostMinus' Expr
  
  -- memory access operator
  | ListIndex' Bs [Int]

  -- load
  | Load' Expr
  
  -- primitive type
  | Str' Bs
  | Int' Int
  | Bool' Bool
  | Float' Float
  | FieldIndex' Int
  
  -- built-in function
  | Exp'  Expr
  | Log'  Expr
  | Sqrt' Expr
  | Sin'  Expr
  | Cos'  Expr
  | Atan2' Expr
  
  | Length' Expr
  | Rand'   Expr
  | SRand'  Expr
  | IntF'   Expr
  | Substr' Expr
  | IndexF' Expr
  
  | Match'   Expr
  | Split'   Expr
  | Subst'   Expr
  | Gsubst'  Expr
  | Sprintf' Expr
  | SystemF' Expr
  | Tolower' Expr
  | Toupper' Expr
  
  deriving (Show,Ord,Eq)

type Record = [Field]
type Field = Bs

-- data Record =
--   Field [Int]
--   deriving (Show)
