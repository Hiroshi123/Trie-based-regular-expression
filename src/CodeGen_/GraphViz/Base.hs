
module Tools.GraphViz.Base
  (
    graphToDot,
    dotWrite
  ) where

--import Prelude hiding (++)

import System.IO
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import Data.Bits 

import Data.Word
import MData.Graph

a +++ b = BS.append a b

graphToDot :: Graph -> [BC.ByteString]
graphToDot g =
  
  [BC.pack "digraph aaa_aaa"] ++
  [BC.pack "{"] ++
  (nodeGet g) ++
  (edgeGet g) ++
  [ BC.pack "}" ]
  
  
data Tree a =
  Empty
  | Leaf a
  | Nod (Tree a) a (Tree a)
  deriving (Show)


instance Foldable Tree where
  
  foldMap f Empty = mempty
  foldMap f (Leaf x) = f x
  foldMap f (Nod l k r) = foldMap f l `mappend` f k `mappend` foldMap f r

instance Functor Tree where
  
  fmap f Empty    = Empty
  fmap f (Leaf x) = Leaf (f x)
  fmap f (Nod l k r) = Nod (fmap f l) (f k) (fmap f r)
  
  
instance Traversable Tree where
  traverse f Empty = pure Empty
  traverse f (Leaf x) = Leaf <$> f x
  traverse f (Nod l k r) = Nod <$> traverse f l <*> f k <*> traverse f r
  
  
--hei :: (Foldable a) => a
--hei = 
  
--treeToDot :: Tree -> [BC.ByteString]
--treeToDot tr = 
  
  
nodeGet :: Graph -> [BC.ByteString]
nodeGet g = map (BS.singleton .  n_label) $ g_node g

edgeGet :: Graph -> [BC.ByteString]
edgeGet g = map (\x -> ((fst x) +++ (BC.pack "->")) +++ (snd x) ) (edgeGet' g)

edgeGet' :: Graph -> [(BC.ByteString, BC.ByteString)]
edgeGet' g = map (\x -> ( BS.singleton $ n_label (fst x) , BS.singleton $ n_label (snd x) ) ) a
  where a = map bridge $ g_edge g
  
  
dotWrite :: FilePath -> [BS.ByteString] -> IO ()
dotWrite fn xs =
  
  openFile fn WriteMode >>=
  (\handle ->
      mapM_ (BC.hPutStrLn handle) xs
      >>= (\_ -> hClose handle)
  )
  

