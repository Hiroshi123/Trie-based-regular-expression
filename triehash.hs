{-# LANGUAGE MultiParamTypeClasses #-}


data Trie2 k v = E2 | N2 Char Int (Trie2 k v) (Trie2 k v)  deriving (Show)


class TrieHash where
  
  --1st argument is query
  --2nd argument is buffer
  --3rd argument is a trie
  --return is trie  
  retrieve  :: String -> Trie2 Char Int-> Maybe (String,Int)
  retrieve' :: String -> String -> Trie2 Char Int-> Maybe (String,Int)
  
  match  :: String -> Trie2 Char Int -> [String]
  match' :: String -> String -> Trie2 Char Int -> [String]
  
  insert :: String -> Trie2 Char Int -> Trie2 Char Int
  
  toList  :: Trie2 Char Int -> [String]
  toList' :: String -> Trie2 Char Int -> [String]
  
  repeat'  :: Char -> String -> String
  repeat'' :: Char -> String -> Trie2 Char Int -> (String , Trie2 Char Int,Bool)
  
  
instance TrieHash where
  
  repeat' _ [] = []
  repeat' s (h:t)
    | s == h = h : (repeat' s t)
    | otherwise = []
    
    
  repeat'' a s E2 = (s,E2,True)
  --repeat'' a s E2 = (s,E2)
  
  
  repeat'' a s (N2 k v b c)
    | a < k  = (s,(N2 k v b c),True)
    | a == k = do
        let (p,q,r) = repeat'' a (s++[a]) b
        (p,q,False)
    | a > k  = do
        let (p,q,r) = repeat'' a s c
        case r of
          True  -> (p,(N2 k v b q),True)
          False -> (p,q,False)
          
        
  retrieve = retrieve' ""
  
  retrieve' s [] (N2 k v b c) = Just (s,1) --Nothing
  retrieve' s _ E2 = Just (s,1) --Nothing
  
  retrieve' s (h:t) (N2 k v b c)
    | (h == k) && (t == []) && (v /= 0) = Just (s++[k],v)
    | h == k   = retrieve' (s++[h]) t b
    | h > k    = retrieve' s (h:t) c
    | h == '.' = retrieve' (s++[k]) t b
    | h == '+' = do
        let (p,q,r) = (repeat'' (head (reverse s)) (s++[k]) (N2 k v b c))
        retrieve' p t q 
    | h < k = Nothing
    
    
  match = match' ""
  match' s [] (N2 k v b E2) = (match' (s++[k]) [] b)
  
  match' s [] (N2 k v b c) = (match' (s++[k]) [] b) ++ (match' s [] c)
  
  match' s [] E2 = []
  match' s _  E2 = []
  
  match' s (h:t) (N2 k v b c)
    | (t /= []) && (head t) == '+' = do
        case h of
          k  -> (match' (s++[k]) (h:t) b) ++ (match' (s++[k]) (tail t) b) 
          _  -> (match' s (h:t) c)
    | (h == '[') = do
        let (m,n) = hei t
        let o = filter (==k) m
        (match' s (h:t) c) ++ (match' (s++o) n b)
        
        
    | (h == k) && (t == []) && (v /= 0) = [s++[k]] ++ (match' (s++[h]) t b)
    | h == k = match' (s++[h]) t b
    | h == '.' = match' (s++[k]) t b
          
    | h > k  = match' s (h:t) c
    | h < k  = []    
    
  insert [] E2 = E2
  insert [] (N2 k v b c) = (N2 k v b c)
  
  insert (h:t) E2
    | t == []   = (N2 h 1 (insert t E2) E2)
    | otherwise = (N2 h 0 (insert t E2) E2)
    
    
  insert (h:t) (N2 k v b c)
    | (h < k) && (t==[]) = (N2 h 1 (insert t E2) (N2 k v b c))
    | h < k = (N2 h 0 (insert t E2) (N2 k v b c))
    | (h == k) && (t==[]) = (N2 k (v+1) (insert t b) c)
    | h == k = (N2 k v (insert t b) c)
    | h > k  = (N2 k v b (insert (h:t) c))  
      
    
  toList = toList' ""
  
  --toList' s E2 = [s]
  toList' s E2 = [s]
  
  toList' s (N2 k 0 b E2) = (toList' (s++[k]) b)
  toList' s (N2 k 0 b c) = (toList' (s++[k]) b) ++ (toList' s c)
  
  toList' s (N2 k v E2 E2) = [s++[k]]
  toList' s (N2 k v b E2) = [s++[k]] ++ (toList' (s++[k]) b)
  
  toList' s (N2 k v b c)  = (toList' (s++[k]) b) ++ (toList' s c)
  
  --toList' s (N2 k v b c) = [s++[k]] ++ (toList' (s++[k]) b) ++ (toList' s c)
  
  
hei :: String -> (String,String)

hei (h:t)
  | h == ']' = ([],t)
  | (head t) == '-' = do
      let m = slash h (head $ tail t)
      let (p,q) = hei (tail t)
      (m++p,q)
      
  | otherwise = do
      let (p,q) = hei t
      (h:p,q)
      
      
alphabet :: String
alphabet = "abcdefghijklmnopqrstuwwxyz"

take' :: Char -> String -> String
take' a (h:t)
  | a == h    = [h]
  | otherwise = h : (take' a t)
  
  
drop' :: Char -> String -> String
drop' a (h:t)
  | a == h    = (h:t)
  | otherwise = drop' a t
  
  
slash :: Char -> Char -> String 
slash a b = take' b (drop' a alphabet)

main = do
  
  let b = N2 'a' 1 E2 E2
  
  print b
  
  print $ toList b
  
  let c = insert "I" b
  
  print $ c
  
  let d = insert "love" c
  let e = insert "like" d
  let f = insert "likes" e
  let g = insert "liiiiked" f
  
  let h = insert "like" g
  let hh = insert "likelyhood" h
  let i = insert "loke" hh
  let z = insert "liked" i
  
  print z
  
  print $ toList z
  print $ retrieve "l.ke" z
  print $ retrieve "li+ked" z
  
  print $ match "li+ked" z
  
  print $ match "l.ked" z

  print $ match "l[i-w]ke" z
  
  print $ match "like" z  
  
  print "hi"
  
  
