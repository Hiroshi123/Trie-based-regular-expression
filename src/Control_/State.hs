


module Control_.State where

import Control_.M 

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC 

--import MData.BNF.Lisp2

import Data.Word
import System.IO

--type Bs = BS.ByteString

data State s a = State {runState :: s -> (a, s)}

instance M (State s) where
  r' x = State $ \s -> (x,s)
  (State f1) >== f2 =
    State $ \s ->
    let (x,s1) = f1 s
    in runState (f2 x) s1
    
  -- for warning prevention
  a **> b = b
  a **< b = a
  a >** b = b
  a <** b = a
    
get :: State s s
get = State $ \s -> (s, s)

put :: s -> State s ()
put s = State $ \_ -> ((), s)

data ST' =
  ST'
  {
    s1 :: [Char],
    s2 :: Int
  }
  deriving (Show)



