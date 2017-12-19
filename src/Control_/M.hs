
module Control_.M where


-- following type class are partially dupricated by the normal type clas of default monad
-- but a little bit extended for a parser

-- (**>) :: m a -> m b -> m b
-- execution order is left(1st argument) to right(2nd argument)
-- if 1st returns false, 2nd would not be executed
-- if both fine, 2nd would be returned

-- (**<) :: m a -> m b -> m a
-- execution order is left(1st argument) to right(2nd argument)
-- if 1st returns false, 2nd would not be executed
-- if both fine, 1st would be returned

  
-- (<**) :: m a -> m b -> m a
-- execution order is right(2nd argument) to left(1st argument)
-- if 1st(right) returns false, 2nd(left) would not be executed
-- if both fine, 2nd(left) would be returned

-- (>**) :: m a -> m b -> m b
-- execution order is right(2nd argument) to left(1st argument)
-- if 1st(right) returns false, 2nd(left) would not be executed
-- if both fine, 1st(right) would be returned


class M m where
  r'    :: a -> m a
  (>==) :: m a -> (a -> m b) -> (m b)
  
  (**>) :: m a -> m b -> m b
  (**<) :: m a -> m b -> m a
  
  (<**) :: m a -> m b -> m a
  (>**) :: m a -> m b -> m b
  
  
class (M m) => MPlus m where
  -- mzero :: m a
  (<>)  :: m a
  --(<|>) :: m a -> m a -> m a
  mmplus :: m a -> m a -> m a

  --(>>>) :: m a -> m b -> m b

instance M (Either a) where
  r' x = Right x
  Left x  >== _ = Left x
  Right x >== f = f x
  
  Left a  **> _ = Left  a
  Right a **> Right b = Right b
  Right a **> Left  b = Left b
  
  Left a  **< _ = Left  a
  Right a **< Right b = Right a
  Right a **< Left  b = Left b
  
  
  _ <** Left  b = Left b
  Right a <** Right b = Right a
  Left  a <** Right b = Left  a
  

  _ >** Left  b = Left b
  
  Right a >** Right b = Right b
  Left a  >** Right b = Left a
  
-- instance M IO where
--   r' = return
--   action >== f =
--     do
--       x <- action
--       f x
      
instance M [] where
  r' x = [x]
  f1 >== f2 = concatMap f2 f1

  --temporary not being pondered :(
  f1 **> f2 = f2
  f1 **< f2 = f1
  f1 <** f2 = f1
  f1 >** f2 = f2
  

              
