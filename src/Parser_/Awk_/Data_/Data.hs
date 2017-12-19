

module Parser_.Awk_.Data_.Data where

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


type VarList = [(Bs,Expr)]

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


data Action = Action' [ [ (State_ -> State_) ] ] --[Statement]

  --deriving (Show)
  
instance Show Action where
  show (Action' s) =
    "number of action over rows is "   ++ show (length s) ++ "\n" ++
    "number of action over column is " ++ show (length (head s)) ++ "\n"
    



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
