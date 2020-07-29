module MaybeLib where

isJust :: Maybe a -> Bool 
isJust (Just _) = True
isJust Nothing = False

isNothing :: Maybe a -> Bool
isNothing = not . isJust

--

mayybee :: b -> (a -> b) -> Maybe a -> b
mayybee _ a2b (Just a) = a2b a
mayybee def _ Nothing = def

--

fromMaybe :: a -> Maybe a -> a
fromMaybe a ma = mayybee a id ma

--

listToMaybe :: [a] -> Maybe a
listToMaybe [] = Nothing
listToMaybe (x:_) = Just x

maybeToList :: Maybe a -> [a]
maybeToList Nothing = []
maybeToList (Just x) = [x]

--

catMaybes :: [Maybe a] -> [a]
catMaybes [] = []
catMaybes xs 
  = map (fromMaybe undefined) 
  $ filter (\x -> isJust x) xs

---

justs :: [Maybe a] -> [a] -> Maybe [a]
justs [] acc = Just $ reverse acc
justs (x:xs) acc
  = case x of
    Just x' -> justs xs (x':acc)
    Nothing -> Nothing

flipMaybe :: [Maybe a] -> Maybe [a]
flipMaybe [] = Just []
flipMaybe xs = justs xs []
