{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE KindSignatures #-}

module MControl.State where

import MControl.M 

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC 

import MData.BNF.Lisp2

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
    
    
fgh :: State Bs [(Bs,Lisp)]
fgh = State (\x -> ( [ ( "" :: Bs , Atom Nil ) ] , x ) )

h1 :: State Bs [Word8]
h1 = State (\x -> ( [ BS.head x ] , x ) )

hh = h1 >== (\x -> h1 >== (\y -> r' x))


--ff = openFile >>= ()

interactFiles f fileNames = do
  ss <- mapM readFile fileNames
  putStr $ f (concat ss)
  
get :: State s s
get = State $ \s -> (s, s)

put :: s -> State s ()
put s = State $ \_ -> ((), s)


--ff :: 
--ff =

--runState :: State s a -> s -> (a,s)
--runState f a = f a

  
