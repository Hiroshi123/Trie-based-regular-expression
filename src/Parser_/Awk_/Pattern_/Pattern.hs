

module Parser_.Awk_.Pattern_.Pattern where

import Control_.M
import Parser_.Base_.Base
import Parser_.Base_.Bool
import Parser_.Base_.ByteStr
import Parser_.Base_.List
import Parser_.Base_.Int


import Parser_.Awk_.Data_.Data

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC



    
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


