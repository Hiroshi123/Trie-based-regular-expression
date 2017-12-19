

module System_.IO where

import System.IO
import System.Directory

--import System.Process


--import Parser_.Base.Preprocess


--import System.Process

import qualified Data.ByteString as BS --(ByteString,unpack)
import qualified Data.ByteString.Char8 as BC



-- unlines_ x = foldr1 (\a b -> BS.append a b) x

-- --pre_ x pss ps = return $ fmap (pss ps) x

-- setDir path = setCurrentDirectory path -- "./example/c/in/"

-- backToHome = getHomeDirectory

-- read_ fn ps =  
--   getCurrentDirectory >>= 
--   (\cd ->
--      withFile (cd ++ "/" ++ fn) ReadMode
--      (\h -> hGetLines h ps Nothing [] >>=
--             (\x -> return x )
            
--             --(\x -> return $ pss ps (unlines_ x ) )
            
--      )
--   )
  
  
-- --hGetLines :: Handle -> IO [BC.ByteString]
-- hGetLines h ps c st =
--   hIsEOF h >>=
--   (\x ->
--       case x of
--         True  -> return st -- []
--         False ->
--           BS.hGetLine h >>=
--           (\cxt -> pre_c h ps c st cxt        
--           )
--   )

-- data Mod =
--   Start [Char] | End [Char]
--   deriving (Show)

-- pre_c h ps c st xx =
--   let fn = parse (init_ (st) >== (\a -> ps c a >== (\b -> r' b ) ) ) xx
--   in case fn of
--        []              -> hGetLines h ps c st  >>= (\xs -> return xs )
--          --where r = split (BC.unpack xx) ':'
--        [(COMMENT,_)]   -> hGetLines h ps c st  >>= (\xs -> return xs )
--        [(MACRO st',_)] -> hGetLines h ps c st' >>= (\xs -> return xs )
--        [(COND a,_)]    -> hGetLines h ps a st  >>= (\xs -> return xs )
--        [(INCLUDE a,_)] -> b >>= (\b_ -> hGetLines h ps c ( fo ++ b_  ) >>= (\xs -> return ( xs ++ l ++ st ) ) )
--          where b  = read_ (BC.unpack a) ps
--                fo = [("--end--"   ,a )]
--                l  = [("--start--" ,a )]
                    
               
-- split a sep = split' a [] sep
              
-- split' [] r _ = ([],r)
-- split' (h:t) buf sep =
--   case  h == sep of
--     True  -> (buf : fs,sn)
--       where a  = split' t [] sep
--             fs = fst a
--             sn = snd a
--     False -> split' t (h : buf) sep
      
-- (+++) = BS.append
  
-- save :: [Char] -> [Char] -> IO ()
-- save fn c =
--   getCurrentDirectory >>= 
--   (\cd ->
--      withFile (cd ++ "/" ++ fn) WriteMode (\h -> BS.hPutStr h $ BC.pack c)
--      -- (\h -> BS.hPutStr h $ BC.pack contents) -- (\h -> BS.hPutStr h $ BC.pack contents)
--   )
  
  

hGetLines :: Handle -> IO [BC.ByteString]
hGetLines h =
  hIsEOF h >>=
  (\x ->
      case x of
        True  -> return []
        False ->
          BS.hGetLine h >>=
          (\x ->             
             --let xx = comment_rem x
             hGetLines h >>=
             (\xs -> return (x:xs))             
          )
  )
  
  
save :: [Char] -> [Char] -> IO ()
save fn c =
  getCurrentDirectory >>= 
  (\cd ->
     withFile (cd ++ "/" ++ fn) WriteMode (\h -> BS.hPutStr h $ BC.pack c)
     -- (\h -> BS.hPutStr h $ BC.pack contents) -- (\h -> BS.hPutStr h $ BC.pack contents)
  )
