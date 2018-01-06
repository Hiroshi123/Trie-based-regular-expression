

data Fruit = Apple | Banana | Else | Pika Int Int deriving (Show)

apple :: Parser [Fruit]
apple = Parser $ \x -> [([Apple],x)]

banana :: Parser [Fruit]
banana = Parser $ \x -> [([Banana],x)]


fru :: Parser [Fruit]
fru = apple <**> banana

conv :: BS.ByteString -> [Fruit]
conv "" = [Else]
conv a =
  case BS.head a of
    97 -> [Apple]
    98 -> [Banana]
    otherwise -> [Else]
    
    
g2 :: Parser [Fruit]
g2 = Parser $
  \x -> case BS.head x of
    97 -> [([Apple ],BS.tail x)]
    98 -> [([Banana],BS.tail x)]
    otherwise -> []


f :: BS.ByteString -> Bool
f x = case BS.head x of 
        100       -> True
        otherwise -> False

dada :: ([Word8],[Fruit])
dada =  ( [100,97,100,97,32],[Banana] )

dddd :: ([Word8] , [Fruit] )
dddd =  ( [100,100,100,100] , [Apple] )

mList x = (string (BS.pack $ fst x),snd x)

gList :: [(Parser BS.ByteString,[Fruit])]
gList = [ mList x | x <- [dada,dddd] ]

g1 :: [(Parser a, [t])] -> BS.ByteString -> [t]
g1 [] _ = []
g1 (h:t) i = case a of
  [] -> g1 t i
  otherwise -> snd h
  where a = parse (fst h) i
  
  --where a = parse (string $ BS.pack word) i
  
    -- "" -> [Else]
    -- otherwise -> [Apple]
    
-- pre1 :: BS.ByteString -> BS.ByteString

pre1 s = parse (satisfy ((BS.pack [32]) ==)) s
pre2 s = parse (many1 (satisfy letter)) s

pre2_1 :: a -> a
pre2_1 a = a

--inputAny = BS.getLine >>= (\x -> return $ parse (string $ g1 gList) x)

--data Trie = Trie [97,98,99

--data Grammer :: ( ByteString -> [Fruit] )

--g1 = Grammer $ (\x -> BS.head x == 97 then [Apple] else [] )

  

