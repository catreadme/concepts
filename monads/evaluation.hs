{-
  A basic evaluator is the basis for the following concepts
-}
-- Integer division
(%) = div

-- A term is either a constant 'Con a' or a quotient 'Div t u'
data Term = Con Int | Div Term Term
  deriving (Show)

-- If the term is a constant, the constant is returned. If the term is a
-- quotient, it's subterms are evaluated and the quotient computed.
eval :: Term -> Int
eval (Con a) = a
eval (Div t u) = (eval t) % (eval u)

-- Examples
j = Con 5
plausible = Div (Con 6) (Div (Con 10) (Con 5))
broken = Div (Con 5) (Con 0)

l = eval j -- 5
m = eval plausible -- 3
n = eval broken -- *** Exception: divide by zero
--

{-
  Concept: Exceptions
-}
-- A M is either an exception 'Raise Exception' or a result 'Return a'
data M a = Raise Exception | Return a
  deriving (Show)
type Exception = String

-- At each call, a check for exception is made. If an exception was raised, it
-- gets re-raised, and if a value was returned, it gets processed further.
eval' :: Term -> M Int
eval' (Con a) = Return a
eval' (Div t u) = case eval' t of
                    Raise e -> Raise e
                    Return a ->
                      case eval' u of
                        Raise e -> Raise e
                        Return b ->
                          if b == 0
                            then Raise "divide by zero"
                            else Return (a % b)

-- Examples
o = Div (Con 5) (Con 0)
p = eval' o -- Raise "divide by zero"

q = Div (Con 5) (Con 1)
r = eval' q -- Return 5
--

{-
  Concept: State
-}
-- A S is a function that takes an initial state 'State' and returns a value
-- with a new state '(a, State)'.
type S a = State -> (a, State)
type State = Int

-- Compute the first value with the initial state, then the second value with
-- the newly obtained state. State increases by 1 for every division.

-- Note that the type signature of eval'' is the same as
--  'Term -> State -> (Int, State)'
--  which is the same as
--  'Term -> Int -> (Int, Int)'
eval'' :: Term -> S Int
eval'' (Con a) x = (a,x)
eval'' (Div t u) x =
  let (a,y) = eval'' t x in
  let (b,z) = eval'' u y in
  (a % b,z+1)

-- Examples
s = Div (Con 5) (Div (Con 1) (Con 1))
t = Con 1

u = eval'' t 0 -- (1,0)
v = eval'' s 0 -- (5,2)
--

{-
  Concept: Output
-}
-- TODO
