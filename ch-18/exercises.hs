import Control.Monad

-- join :: Monad m => m (m a) -> m a
-- fmap :: Functor f => (a -> b) -> f a -> f b

bind :: Monad m => (a -> m b) -> m a -> m b 
bind f m = join $ fmap f m

-- Implement the Either Monad

data Sum a b 
  = First a
  | Second b
  deriving (Eq, Show)

instance Functor (Sum a) where
  fmap f (Second b) = Second $ f b
  fmap _ (First a) = First a

instance (Monoid a) => Applicative (Sum a) where
  pure = Second
  First a <*> First a' = First $ a <> a'
  Second f <*> Second b = Second $ f b
  First a <*> Second _ = First a
  Second _ <*> First a = First a

instance (Monoid a) => Monad (Sum a) where 
  return = pure
  Second b >>= f = f b
  First a >>= _ = First a


mcomp :: Monad m => (b -> m c) -> (a -> m b) -> a -> m c 
mcomp b2mc a2mb a = join $ fmap b2mc (a2mb a)