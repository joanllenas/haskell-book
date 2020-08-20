module Exercises where

functorIdentity :: (Functor f, Eq (f a)) => f a -> Bool
functorIdentity f = fmap id f == f

functorCompose :: (Eq (f c), Functor f) => (a -> b) -> (b -> c) -> f a -> Bool
functorCompose f g x = (fmap g (fmap f x)) == (fmap (g . f) x)

--

newtype Constant a b 
  = Constant { getConstant :: a }
  deriving (Eq, Show)

instance Functor (Constant a) where
  fmap _ (Constant a) = Constant a

{-
λ> functorIdentity (Constant 3)
True
λ> functorIdentity (Constant "hola")
True
λ> functorCompose (++"A") reverse (Constant "hola")
True
-}

--

data Wrap f a 
  = Wrap (f a)
  deriving (Eq, Show)

instance (Functor f) => Functor (Wrap f) where
  fmap f (Wrap fa) = Wrap (fmap f fa)

{-
Prelude> fmap (+1) (Wrap (Just 1))
Wrap (Just 2)
Prelude> fmap (+1) (Wrap [1, 2, 3])
Wrap [2,3,4]
-}


getInt :: IO Int
getInt = fmap read getLine

main :: IO ()
main = do
  a <- getInt
  print (a + 2)


--

data Sum a b 
  = First a
  | Second b

instance Functor (Sum a) where 
  fmap _ (First a) = First a
  fmap f (Second b) = Second (f b)

--

data Company a b c 
  = DeepBlue a c
  | Something b

instance Functor (Company a b) where 
  fmap f (DeepBlue a c) = DeepBlue a (f c)
  fmap _ (Something b) = Something b

--

data More a b 
  = L a b a
  | R b a b 
  deriving (Eq, Show)

instance Functor (More a) where
  fmap f (L a b a') = L a (f b) a'
  fmap f (R b a b') = R (f b) a (f b')

--

data Quant a b 
  = Finance
  | Desk a 
  | Bloor b

instance Functor (Quant a) where
  fmap _ Finance = Finance
  fmap _ (Desk a) = Desk a
  fmap f (Bloor b) = Bloor (f b)

--

data K a b = K a

instance Functor (K a) where
  fmap _ (K a) = K a

--

data LiftItOut f a 
  = LiftItOut (f a)

instance (Functor f) => Functor (LiftItOut f) where
  fmap f (LiftItOut fa) = LiftItOut $ fmap f fa

--

data Parappa f g a 
  = DaWrappa (f a) (g a)

instance (Functor f, Functor g) => Functor (Parappa f g) where
  fmap f (DaWrappa fa ga) = DaWrappa (fmap f fa) (fmap f ga)

--

data IgnoreOne f g a b = IgnoringSomething (f a) (g b)

instance (Functor g) => Functor (IgnoreOne f g a) where
  fmap f (IgnoringSomething fa gb) = IgnoringSomething fa (fmap f gb)

--

data Notorious g o a t = Notorious (g o) (g a) (g t)

-- Doesn't compile, why?
--instance (Functor g) => Functor (Notorious g o a) where
--  fmap f (Notorious go ga gt) = Notorious (fmap f go) (fmap f ga) (fmap f gt)

instance (Functor g) => Functor (Notorious g o a) where
  fmap f (Notorious go ga gt) = Notorious go ga (fmap f gt)


--

data List a 
  = Nil
  | Cons a (List a)

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons a list) = Cons (f a) (fmap f list)

--

data GoatLord a 
  = NoGoat
  | OneGoat a
  | MoreGoats (GoatLord a)
              (GoatLord a)
              (GoatLord a)

instance Functor GoatLord where
  fmap _ NoGoat = NoGoat
  fmap f (OneGoat a) = OneGoat (f a)
  fmap f (MoreGoats gl1 gl2 gl3) = MoreGoats (fmap f gl1) (fmap f gl2) (fmap f gl3)

--

data TalkToMe a 
  = Halt
  | Print String a
  | Read (String -> a)

instance Functor TalkToMe where
  fmap _ Halt = Halt
  fmap f (Print s a) = Print s (f a)
  fmap f (Read s2a) = Read (f . s2a)
