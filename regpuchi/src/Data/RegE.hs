
module RegE where

import Data.Word

data RegE =
  Empty
  | Conj RegE RegE
  | Disj RegE RegE
  | Star RegE
  | Symbol Word8
  deriving (Show)

data RegVM =
  Step Char
  | Jump Int
  | Split Int Int
  | Match
  deriving (Show)

