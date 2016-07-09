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

{-
  Concept: Exceptions
-}
-- The exception monad
data E a = Raise Exception | Return a
  deriving (Show)
type Exception = String
-- Put a norma value into the exception "context"
unite :: a -> E a
unite a = Return a
-- Apply a value 'E a' to a function of type 'a -> E b' to get a result 'E b'
-- which can be a success 'Return a' or a failure 'Raise e'
(★★) :: E a -> (a -> E b) -> E b
m ★★ k = case m of
  Raise e -> Raise e
  Return a -> k a
-- Raise an exception into the monadic "context"
raise :: Exception -> E a
raise e = Raise e

-- If the term is a constant, the constant is returned as a 'Result a'. If the
-- term is a quotient, it's subterms are evaluated and the quotient computed.
-- If there is a "divide by zero" exception a 'Raise e' is returned, otherwise
-- a 'Return a'
evale :: Term -> E Int
evale (Con a) = unite a
evale (Div t u) = evale t ★★ \x -> evale u ★★ \y ->
  if y == 0
  then raise "divide by zero"
  else unite (x % y)

-- Examples
o = Div (Con 5) (Con 0)
p = evale o -- Raise "divide by zero"

q = Div (Con 5) (Con 1)
r = evale q -- Return 5

{-
  Concept: State
-}
-- The state monad
type S a = State -> (a, State)
type State = Int
-- Put a norma value into the state "context"
units :: a -> S a
units a = \x -> (a,x)
-- Apply a value 'S a' to a function of type 'a -> S b' to get a result 'S b'
-- along with a state
-- '★★★' :: (Int -> (a, Int)) -> (a -> (Int -> (b, Int))) -> (Int -> (b,Int))
(★★★) :: S a -> (a -> S b) -> S b
m ★★★ k = \x -> let (a,y) = m x in
                let (b,z) = k a y in
                (b,z)

-- Change the state by adding one and not modifiying the result
tick :: S ()
tick = \x -> ((), x + 1)

-- If the term is a constant, the "constant taking a state" is returned.
-- If the term is a quotient, it's subterms are evaluated with their states.
-- At the end, the result is computed and the definitive state passed along.
evals :: Term -> S Int
evals (Con a) = units a
evals (Div t u) = evals t ★★★ \x -> evals u ★★★ \y -> tick ★★★ \_ -> units (x % y)

-- Examples
s = Div (Con 5) (Div (Con 1) (Con 1))
t = Con 1

u = evals t 0 -- (1,0)
v = evals s 0 -- (5,2)
