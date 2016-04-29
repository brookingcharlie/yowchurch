{-# LANGUAGE RankNTypes #-}

module Ex2b_ChurchNat where
  
  
-- "r -> r" handles a successor to another nat. "Do this N times..."
-- "r" handles zero.  "...starting from here"
newtype CNat = CNat 
  { cFold :: forall r. (r -> r) -> r -> r 
  }
  
c0, c1, c2, c3, c4 :: CNat 

c0 = CNat $ \f -> id 
c1 = CNat $ \f -> f 
c2 = CNat $ \f -> f . f
c3 = CNat $ \f -> f . f . f
c4 = CNat $ \f -> f . f . f . f


-- Ex 2b.1: Add one to a Church numeral
cSucc :: CNat -> CNat
cSucc (CNat nTimes) = CNat $ \f -> (nTimes f) . f

-- Ex 2b.2: Implement Church addition
-- Can you make it run in constant time?
infixl 6 .+
(.+) :: CNat -> CNat -> CNat
(.+) (CNat nTimes) (CNat mTimes) = CNat $ \f -> (nTimes f) . (mTimes f)

-- Ex 2b.3: Implement Church multiplication
-- Can you make it run in constant time? 
infixl 7 .*
(.*) :: CNat -> CNat -> CNat
(.*) (CNat nTimes) (CNat mTimes) = CNat $ nTimes . mTimes

-- Ex 2b.4: Implement Church exponentiation
infixr 8 .^
(.^) :: CNat -> CNat -> CNat
(.^) (CNat nTimes) (CNat mTimes) = CNat $ mTimes nTimes

-- Ex 2b.5: Convert a Church numeral to an integer
unchurch :: CNat -> Int
unchurch (CNat f) = f (+1) 0

-- Ex 2b.6: Convert a non-negative integer to a Church numeral.
church :: Int -> CNat
church n = (iterate cSucc c0) !! n




-- Instance boilerplate
instance Show CNat where
  show = ("church " ++) . show . unchurch
  
instance Eq CNat where
  a == b = unchurch a == unchurch b  


