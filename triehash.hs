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
  
  inBraceL :: String -> (String,String)
  
  
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
  -- +(repeat -> more than 1)
    | (t /= []) && (head t) == '+' && (h == k) = (match' (s++[k]) (h:t) b) ++ (match' (s++[k]) (tail t) b)
    | (t /= []) && (head t) == '+' = match' (s++[k]) (h:t) c
    
    
  -- \*(repeat -> more than 0)
    | (t /= []) && (head t) == '*' && (h == k) = (match' (s++[k]) (h:t) b) ++ (match' s (tail t) b)
    | (t /= []) && (head t) == '*' = (match' s (tail t) (N2 k v b c)) ++ (match' s (h:t) c)
    
    
  -- {} (fixed repeat)  
    | (t /= []) && (head t) == '{' && (head $ tail t) /= '0' && (h == k) = do
        let ls = head $ tail t
        let tt = "{" ++ [oneDown ls] ++ tail (tail t)
        match' (s++[k]) (h:tt) b
    | (t /= []) && (head t) == '{' && (head $ tail t) /= '0' = match' s (h:t) c
    | (t /= []) && (head t) == '{' && (head $ tail t) == '0' = match' s (tail $ tail $ tail t) (N2 k v b c)
    
                                                               
    --brace (or)
    | (h == '[') = do
        let (m,n) = inBraceL t
        let o = filter (==k) m
        (match' s (h:t) c) ++ (match' (s++o) n b)
        
    --digit
    | (h == '\\') && ((head t) == 'd') = do
        let o = filter (==k) digit
        case o of
          [] -> match' s (h:t) c
          _  -> (match' (s++o) (tail t) b) ++ (match' s (h:t) c)
          
          
    --large capital
    | (h == '\\') && ((head t) == 'w') = do
        let o = filter (==k) largeCapital
        case o of
          [] -> match' s (h:t) c
          _  -> match' (s++o) (tail t) b ++ (match' s (h:t) c)
          
          
    -- match and the end of word and query
    | (h == k) && (t == []) && (v /= 0) = [s++[k]] ++ (match' (s++[h]) t b)
    -- match but not meet both two condition
    | h == k = match' (s++[h]) t b
    -- jump (no matter what matches)
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
  
  inBraceL (h:t)
    | h == ']' = ([],t)
    | (head t) == '-' = do
        let m = slash h (head $ tail t)
        let (p,q) = inBraceL (tail t)
        (m++p,q)
    | otherwise = do
        let (p,q) = inBraceL t
        (h:p,q)
        
        
alphabet :: String
alphabet = "abcdefghijklmnopqrstuwwxyz"

digit :: String
digit = "0123456789"

oneDown :: Char -> Char
oneDown = oneDown' (reverse digit)
oneDown' :: String -> Char -> Char
oneDown' (h:t) a
  | h == a    = head t
  | otherwise = oneDown' t a


largeCapital :: String
largeCapital = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"


takeL :: Char -> String -> String
takeL a (h:t)
  | a == h    = [h]
  | otherwise = h : (takeL a t)
  
  
dropL :: Char -> String -> String
dropL a (h:t)
  | a == h    = (h:t)
  | otherwise = dropL a t
  
  
slash :: Char -> Char -> String 
slash a b = takeL b (dropL a alphabet)


charInt :: [(Char,Int)]
charInt =
  [
  ('0',0),('1',1),('2',2),('3',3),('4',4),
  ('5',5),('6',6),('7',7),('8',8),('9',9)
  ]

charToInt :: Char -> Int
charToInt a = snd $ head $ filter (\(x,y)-> x == a) charInt

main = do
  
  let b = N2 'a' 1 E2 E2
  
  print b
  
  print $ toList b
  
  let c = insert "I" b
  
  print $ c
  
  let d = insert "Love" c
  let e = insert "liked" d
  let f = insert "likes" e
  let g = insert "liiiked" f
  
  let h = insert "like" g
  let hh = insert "likelyhood" h
  let i = insert "loke" hh
  let j = insert "sea1cj" i
  let z = insert "lked" j
  
  print z
  {--
  print $ toList z
  print $ retrieve "l.ke" z
  print $ retrieve "li+ked" z
  
  print $ match "li+ked" z
  
  print $ match "l.ked" z

  print $ match "l[i-w]ke" z

  print $ match "sea\\dcj" z
  
  print $ match "like" z
  
  print $ match "\\wove" z
  --}

  print $ match "li{1}ke" z
  print $ match "li{3}ked" z
  
  print $ match "li+ked" z
  
  print $ charToInt '0'
  --print $ ([oneDown '7']++"}")
  --let o = filter (=='L') largeCapital
  
  print "hi"
  
  
