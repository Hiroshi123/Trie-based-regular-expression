

module Parser_.Awk_.Function_.Function where

import Parser_.Awk_.Data_.Data

import Control_.M
import Parser_.Base_.Base
import Parser_.Base_.Bool
import Parser_.Base_.ByteStr
import Parser_.Base_.List
import Parser_.Base_.Int

import Parser_.Awk_.Action_.Statement_.Assign
import Parser_.Awk_.Action_.Statement_.Print

function_ :: Parser Function
function_ =
  with_space anyLetters >==
  (\fn ->
      (
        sepby
        (assignSt <|> print_ )
        (char ";")
      ) >== (\x -> r' (Function' fn x))
  



