{-# LANGUAGE RankNTypes #-}

import Prelude hiding (Functor, (<$>), fmap)

class Functor (f :: * -> *) where
  (<$>) :: (a -> b) -> f a -> f b

infixr 6 <$>

instance Functor [] where
  f <$> [] = []
  f <$> (x:xs) = f x : f <$> xs

instance Functor Maybe where
  f <$> Nothing = Nothing
  f <$> Just x  = Just (f x)

data FunctorI f = FunctorI {
  fmap :: forall a b . (a -> b) -> f a -> f b }

_ListFunctorI :: FunctorI []
_ListFunctorI  = FunctorI { fmap = helper }
  where helper f []     = []
        helper f (x:xs) = f x : helper f xs

_MaybeFunctorI :: FunctorI Maybe
_MaybeFunctorI  = FunctorI { fmap = helper }
  where helper f Nothing  = Nothing
        helper f (Just x) = Just (f x)

-- λ> (+1) <$> [1..100] == fmap _ListFunctorI (+1) [1..100]
-- True

(<$$>) :: Functor f => (a -> a) -> f a -> f a
f <$$> box = f <$> f <$> box

fmap2 :: FunctorI f -> (a -> a) -> f a -> f a
fmap2 i f x = fmap' f (fmap' f x)
  where fmap' = fmap i

-- λ> (+1) <$$> [1..100] == fmap2 _ListFunctorI (+1) [1..100]
-- True
