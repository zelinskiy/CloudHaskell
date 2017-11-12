class Summable a where
  add :: a -> a -> a

instance Summable Int where
  add a b = a + b

instance Summable [a] where
  add xs ys = xs ++ ys

{-
main = do
  print (1 `add` 2)
  print $ [1,2,3] `add` [4,5,6]
-}

main = do
  print ((1) `add` 2)
  print $ [1,2,3] `add` [4,5,6]
