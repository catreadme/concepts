-- Monad
data M a = M a
  deriving (Show, Eq)

-- Unit
unit :: a -> M a
unit a = M a

-- Bind
(★) :: M a -> (a -> M b) -> M b
(M a) ★ k = k a

{-
  Laws
-}
-- unit a ★ f = f a
leftUnit = unit 1 ★ (\x -> unit $ x + 1) == (\x -> unit $ x + 1) 1
-- True

-- a ★ unit = a
rightUnit = M 1 ★ unit == M 1
-- True

-- (a ★ f) ★ g = a ★ \x -> f x ★ g
associative =
  (M 1 ★ (\x -> unit $ x + 1)) ★ (\x -> unit $ x - 1) ==
  M 1 ★ \x -> (\x -> unit $ x + 1) x ★ (\x -> unit $ x - 1)
-- True
