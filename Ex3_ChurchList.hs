{-# LANGUAGE RankNTypes #-}

module Ex3_ChurchList where

-- "a -> r -> r" handles a head element and tail list  
-- "r" handles an empty list
newtype CList a = CList 
  { cFoldr :: (forall r. (a -> r -> r) -> r -> r)
  }

-- Ex 3.1: Implement the empty Church list
cNil :: CList a  
cNil = CList $ \ x y -> y

-- Ex 3.2: Implement Church "cons", which 
-- joins a head value to a tail list, forming a new list.
infixr 5 .:
(.:) :: a -> CList a -> CList a
(.:) a (CList f) = CList $ \ cons nil -> cons a (f cons nil)

-- Ex 3.3: Implement Church append, joining two lists together. 
-- Hint: Consider how you might implement the non-church append
-- (++) :: [a] -> [a] -> [a]
-- Response: I'd implement non-church append as follows
-- (++) xs ys = foldr (:) ys xs
infixr 5 .++
(.++) :: CList a -> CList a -> CList a
(.++) x y = undefined

-- Ex 3.4: Take the length of a Church list.
cLength :: CList a -> Int
---- Tried to take same approach as with unchurch function
---- But how to make CList out of y? ----------v
--cLength (CList f) = f (\ x y -> 1 + (cLength y)) 0
cLength (CList f) = undefined

-- 3.5: Convert a Church list to a regular list.
unchurch :: CList a -> [a]
unchurch (CList f) = f (\ x y -> x : y) []

-- 3.6: Convert a regular list to a Church list.
church :: [a] -> CList a
church [] = cNil
church (x:xs) = x .: (church xs)



-- Instances boilerplate
instance Show a => Show (CList a) where 
  show = ("church " ++) . show . unchurch   

instance Eq a => Eq (CList a) where
  a == b = unchurch a == unchurch b 
