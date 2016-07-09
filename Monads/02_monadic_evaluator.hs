{-
  A monadic evaluator is the solution to the previous concepts
-}
-- Integer division
(%) = div

-- A term is either a constant 'Con a' or a quotient 'Div t u'
data Term = Con Int | Div Term Term
  deriving (Show)

-- The identity monad
type M a = a
-- Put a norma value into the monadic "context"
unit :: a -> M a
unit a = a
-- Apply a value 'M a' to a function of type 'a -> M b' to get a result 'M b'
(★) :: M a -> (a -> M b) -> M b
a ★ k = k a

-- If the term is a constant, the constant is returned. If the term is a
-- quotient, it's subterms are evaluated and the quotient computed
eval :: Term -> M Int
eval (Con a) = unit a
eval (Div t u) = eval t ★ \x -> eval u ★ \y -> unit (x % y)

-- Examples
-- Note that the result types of 'l' ana 'm' are 'M Int'
j = Con 5
k = Div (Con 6) (Div (Con 10) (Con 5))

l = eval j -- 5
m = eval k -- 3
