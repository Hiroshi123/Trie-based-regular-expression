
module MData.M where

class M m where
  r'    :: a -> m a
  (>==) :: m a -> (a -> m b) -> (m b)
  
class (M m) => MPlus m where
  -- mzero :: m a
  (<>)  :: m a
  --(<|>) :: m a -> m a -> m a
  mmplus :: m a -> m a -> m a


  
