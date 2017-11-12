{-# LANGUAGE GADTs #-}

data N where
  Z :: N
  S :: N -> N

add :: (N, N) -> N
add (a, S b) = S (add (a, b))
add (a, Z)   = a

five = add (S (S Z), (S (S (S Z))))
