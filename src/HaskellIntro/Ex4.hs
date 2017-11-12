import Prelude hiding (Monad, (>>=), (>>), return)

class Monad m where
  (>>=) :: m a -> (a -> m b) -> m b
  return :: a -> m a

(>>) a b = a >>= \_ -> b

newtype State s a = State { runState :: s -> (a, s) }

get :: State s s
get = State $ \st -> (st, st)

put :: s -> State s ()
put newState = State $ \_ -> ((), newState)

instance Monad (State s) where
  return x = State (\s -> (x, s))
  p >>= k = q where
    p' = runState p 
    k' = runState . k
    q' s0 = (y, s2) where
        (x, s1) = p' s0
        (y, s2) = k' x s1
    q = State q'

data RealWorld = RealWorld

type IO a = State RealWorld a

ex1 :: State (Int, Int) Int
ex1 =
  put (1,2)
  >> get
  >>= (\(a,b) -> return a)
  
