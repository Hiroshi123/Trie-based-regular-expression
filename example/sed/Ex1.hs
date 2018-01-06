

import System.IO  
import System_.IO -- (hGetLines)

--import System.Process

import System.Directory

import Parser_.Base_.Base

-- path = "example/json/in/"

-- unlines_ x = foldr1 (\a b -> BS.append a b) x

-- --g = save "example/json/out/dot/ex1.dot" ff

-- parse_ x = parse jvalue x

-- tm x = save "example/json/out/dot/ex1.dot" a
-- --tm x = save "example/json/out/dot/ex1.dot" a
--   where a = ff x
  
-- parse__ x = fst $ head $ parse_ (unlines_ x)

-- -i
-- -e
--

-- sed [-Ealn] command [file ...]
-- sed [-Ealn] [-e command] [-f command_file] [-i extension] [file ...]


-- data Sed =
--   Sed'  Commnad FilePath
--   Sed'' Commnad CommnadFile Extension FilePath
--   deriving (Show)

-- cmd_ :: [ String ] -> (Option,Expression,FilePath)
-- cmd_ a = a

import Control_.M
import Parser_.Base_.Base
import Parser_.Base_.Bool
import Parser_.Base_.ByteStr
import Parser_.Base_.Int

import Parser_.Sed_.Base

-- address_ :: State -> f -> State
-- address_ st f =
--   case ln == 1 of
--     True  -> f 
--     False ->

-- "1d"

-- f :: Bs -> Bs
-- d :: Bs -> ""

-- d_ x = ""

-- ju =
--   (<->) **> num >==
--   (\x -> (<->) **> ju_ >== (\y -> r' (x,y) ) ) -- <|> r' x)

data State'_ = State'_
  {
    --CONVFMT
    --convfmt :: [Char],
    ln :: Int,
    ps :: PatternSpace,
    hs :: HoldSpace
    
  } deriving (Show)

ju_ =
  (
    (<->) **> num >==
    (\x -> char "," **> (<->) **> num >== (\y -> r' (x,y)))  
  )  <|> ( num >== (\x -> r' (x,x)) )
  
  
g = num >==
  (\x -> char "d" >==
    (\_ -> r' (x,(\a -> "") )
    )
  )
  
-- unlines_ x = foldr1 (\a b -> BS.append a b) x

type Address_ = Int

-- data F_ = Bs -> Bs

ff :: ( Address_ , (Bs -> Bs) ) -> Bs -> State'_ -> State'_
ff (adr,f) x st =
  case address'_ adr x st of
    True  -> State'_
      {
        ln = ln_,
        ps = f x : ps st,
        hs = hs st
      }
      
    False -> State'_
      {
        ln = ln_,
        ps = x : (ps st),
        hs = hs st
      }
      
  where ln_ = (ln st) + 1
  
address'_ :: Address_ -> Bs -> State'_ -> Bool
address'_ adr x st =
  --(ln st) == adr
  
  case a == adr of
    True  -> True
    False -> False
  where a = ln st
  
read_ sc fn =  
  getCurrentDirectory >>= 
  (\cd ->
     withFile (cd ++ "/" ++ fn) ReadMode
     (\h -> hGetLines h >>=
            (\x -> return $ foldl (\s a -> ff (adr,f) a s) init_state_ x)
            
            -- (\x -> return $ unlines_ x)
            
            -- (\x -> return $ foldl (\s a -> s ++ a) x)
            
            -- (foldl (\s a ->
              --           let buf = (snd s) ++ ( fst $ head (parse ps a) )
              --           in ( (fst s) , buf ) ) (init_state_,[]) x
              -- )
            
     )
  )
  
  where (adr,f) = fst $ head $ parse g sc
  
          
init_state_ :: State'_
init_state_ =
  State'_
  {
    ln = 1,
    ps = [],
    hs = []
  }
  
-- main argv =
--   withFile (argv ++ "ex1.txt") ReadMode
--   (\h -> hGetLines h >>=
--     (
--       \x -> return x --mapM_ print (f x)
--     )
--   )
  
  
-- g_path = "./example/json/out/"

-- svgout = rawSystem "dot" [g_path ++ "dot/ex1.dot","-T","svg","-o",g_path ++ "svg/ex1.svg"]

-- open = rawSystem "open"  [ g_path ++ "svg/ex1.svg" ]


  
