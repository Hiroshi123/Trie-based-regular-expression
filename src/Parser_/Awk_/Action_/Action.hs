

module Parser_.Awk_.Action_.Action where

import Parser_.Awk_.Data_.Data

import Control_.M
import Parser_.Base_.Base
import Parser_.Base_.Bool
import Parser_.Base_.ByteStr
import Parser_.Base_.List
import Parser_.Base_.Int

import Parser_.Awk_.Statement_.Assign
import Parser_.Awk_.Statement_.Print
import Parser_.Awk_.Statement_.Cond

action_ :: Parser Action
action_ =
  (
    sepby
    (assignSt <|> print_ ) -- <|> if_ )
    (char ";")
  ) >== (\x -> r' (Action' x))



