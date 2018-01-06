
module Lib
  (
    someFunc
  ) where

-- import qualified Data.ByteString as BS --(ByteString,unpack)
-- import qualified Data.ByteString.Char8 as BC --(ByteString,unpack)

-- -- you need type constructor which forms Monad instance

-- import MData.M

-- -- At first, you are going to perse input-text.

-- -- Monadplus ( Disjunction ) and Bind Operation ( Conjunction )
-- -- will be defined in Base.hs

-- import MData.Parser.Base

-- -- when you construct an AST for regular expression,
-- -- you need to set Parser which would return Regular Expression Data Type.

-- import MData.Parser.RegEP

-- -- grammer of regular expression
-- -- in order to construct its grammer, you have to define the BNF of regular expression
-- -- also, to visualize its output, you need conversion script to Graph Data Structure.

-- import MData.RegE

-- -- after being converted to graph data structure, it should be converted to dot language.
-- -- display as a dot language

-- import Tools.GraphViz.Base

--dotW "hei.dot" $ graphToDot $ toGraph $ Star (C 100)
-- "*"
-- "^"
-- ""
--
--

--input = BS.getLine >>= (\x -> return $ fff x)

--fff x = dotW "heij.dot" $ graphToDot $ toGraph $ a

--fff :: BC.ByteString -> [BC.ByteString]
--fff x = graphToDot $ toGraph $ a
--  where a = fst $ head $ parse rule1 x

someFunc :: IO ()
someFunc = putStrLn "someFunc"

