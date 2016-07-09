{-
  A simple evaluator is the basis for the following concepts
-}
-- Integer division
(%) = div

-- A term is either a constant 'Con a' or a quotient 'Div t u'
data Term = Con Int | Div Term Term
  deriving (Show)

-- If the term is a constant, the constant is returned. If the term is a
-- quotient, it's subterms are evaluated and the quotient computed
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

{-
  Concept: Exceptions
-}
-- A M is either an exception 'Raise Exception' or a result 'Return a'
data E a = Raise Exception | Return a
  deriving (Show)
type Exception = String

-- At each call, a check for exception is made. If an exception was raised, it
-- gets re-raised, and if a value was returned, it gets processed further
evale :: Term -> E Int
evale (Con a) = Return a
evale (Div t u) = case evale t of
                    Raise e -> Raise e
                    Return a ->
                      case evale u of
                        Raise e -> Raise e
                        Return b ->
                          if b == 0
                            then Raise "divide by zero"
                            else Return (a % b)

-- Examples
o = Div (Con 5) (Con 0)
p = evale o -- Raise "divide by zero"

q = Div (Con 5) (Con 1)
r = evale q -- Return 5

{-
  Concept: State
-}
-- A S is a function that takes an initial state 'State' and returns a value
-- with a new state '(a, State)'
type S a = State -> (a, State)
type State = Int

-- Compute the first value with the initial state, then the second value with
-- the newly obtained state. State increases by 1 for every division

-- Note that the type signature of evals is the same as
--  'Term -> State -> (Int, State)'
--  which is the same as
--  'Term -> Int -> (Int, Int)'
evals :: Term -> S Int
evals (Con a) x = (a,x)
evals (Div t u) x =
  let (a,y) = evals t x in
  let (b,z) = evals u y in
  (a % b,z+1)

-- Examples
s = Div (Con 5) (Div (Con 1) (Con 1))
t = Con 1

u = evals t 0 -- (1,0)
v = evals s 0 -- (5,2)
--

{-
  Concept: Output
-}
-- An O is an Output paired with the result of a computation
type O a = (Output,a)
type Output = String

-- Compute the first value with the output, then the second value with the
-- new output appended to the old one
evalo :: Term -> O Int
evalo (Con a) = (line (Con a) a, a)
evalo (Div t u) =
  let (x,a) = evalo t in
  let (y,b) = evalo u in
  (x ++ y ++ line (Div t u) (a%b),a%b)

-- Generates one line of Output
line :: Term -> Int -> Output
line t a = "eval (" ++ show t ++ ") <= " ++ show a ++ "\n"

-- Examples

a' = (Con 5)
b' = Div (Con 5) (Div (Con 10) (Con 2))

c' = putStr $ fst $ evalo a'
-- eval (Con 5) <= 5

d' = putStr $ fst $ evalo b'
-- eval (Con 5) <= 5
-- eval (Con 10) <= 10
-- eval (Con 2) <= 2
-- eval (Div (Con 10) (Con 2)) <= 5
-- eval (Div (Con 5) (Div (Con 10) (Con 2))) <= 1
