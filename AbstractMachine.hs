import Prelude hiding (Num)
import OperationalSemantics

data Inst = PUSH Num | ADD | MULT | SUB | AM_TRUE | AM_FALSE | EQUALS | LE | AND | NEG | FETCH Var | STORE Var |
            NOOP | BRANCH Code Code | LOOP Code Code
            deriving (Show, Eq)

type Code = [Inst]

type Storage = State

data Expression = INT Z | BOOL T 
    deriving(Show)

type Stack = [Expression]

type Config = (Code, Stack, Storage)

am_step :: Config -> Config
am_step ([], e, s) = ([], e, s)
am_step ((PUSH n):c, e, s) = (c, n_expr:e, s) where
    n_expr = INT (n_val n)
am_step (ADD:c, (INT z1):(INT z2):e, s) = (c, n_expr:e, s) where
    n_expr = INT (z1 + z2)
am_step (MULT:c, (INT z1):(INT z2):e, s) = (c, n_expr:e, s) where
    n_expr = INT (z1 * z2)
am_step (SUB:c, (INT z1):(INT z2):e, s) = (c, n_expr:e, s) where
    n_expr = INT (z1 - z2)
am_step (AM_TRUE:c, e, s) = (c, b_expr:e, s) where
    b_expr = BOOL True
am_step (AM_FALSE:c, e, s) = (c, b_expr:e, s) where
    b_expr = BOOL False
am_step (EQUALS:c, (INT z1):(INT z2):e, s) = (c, b_expr:e, s) where
    b_expr = BOOL (z1 == z2)
am_step (LE:c, (INT z1):(INT z2):e, s) = (c, b_expr:e, s) where
    b_expr = BOOL (z1 <= z2)
am_step (AND:c, (BOOL b1):(BOOL b2):e, s) = (c, b_expr:e, s) where
    b_expr = BOOL (b1 && b2)
am_step (NEG:c, (BOOL b):e, s) = (c, b_expr:e, s) where
    b_expr = BOOL (not b)
am_step ((FETCH x):c, e, s) = (c, n_expr:e, s) where
    n_expr = INT (s x)
am_step ((STORE x):c, (INT z):e, s) = (c, e, s') where
    s' = update s z x
am_step (NOOP:c, e, s) = (c, e, s)
am_step ((BRANCH c1 c2):c, (BOOL b):e, s) = (c_expr ++ c, e, s) where
    c_expr | b         = c1
           | otherwise = c2
am_step ((LOOP c1 c2):c, e, s) = (c_expr ++ c, e, s) where
    c_expr = c1 ++ [BRANCH (c2 ++ [LOOP c1 c2]) [NOOP]]

am_comp_seq :: Code -> State -> [Config]
am_comp_seq code s = am_steps (code, [], s) where
    am_steps :: Config -> [Config]
    am_steps ([], e, s) = [([], e, s)]
    am_steps config = config:(am_steps (am_step config))

run :: Code -> State -> State
run code s = s' where
    (_, _, s') = last $ am_comp_seq code s


ca :: Aexp -> Code
ca (N n)      = [PUSH n]
ca (V x)      = [FETCH x]
ca (Add a1 a2)  = (ca a2) ++ (ca a1) ++ [ADD]
ca (Sub a1 a2)  = (ca a2) ++ (ca a1) ++ [SUB]
ca (Mult a1 a2) = (ca a2) ++ (ca a1) ++ [MULT]

cb :: Bexp -> Code
cb TRUE        = [AM_TRUE]
cb FALSE       = [AM_FALSE]
cb (Neg b)     = (cb b) ++ [NEG]
cb (And b1 b2) = (cb b2) ++ (cb b1) ++ [AND]
cb (Eq a1 a2)  = (ca a2) ++ (ca a1) ++ [EQUALS]
cb (Le a1 a2)  = (ca a2) ++ (ca a1) ++ [LE]

cs :: Stm -> Code
cs (Ass v a) = (ca a) ++ [STORE v]
cs (Skip) = [NOOP]
cs (Comp s1 s2) = (cs s1) ++ (cs s2)
cs (If b s1 s2) = (cb b) ++ [BRANCH (cs s1) (cs s2)]
cs (While b s) = [LOOP (cb b) (cs s)]

testStatement :: Stm
testStatement = (Comp(Ass "y" (N 1)) (While (Neg (Eq (V "x")(N 1)))(Comp(Ass "y" (Mult (V "y") (V "x")))(Ass "x" (Sub (V "x") (N 1))))))

testCode :: Code
testCode = [PUSH 1,STORE "y",LOOP [PUSH 1, FETCH  "x", EQUALS, NEG][FETCH  "x", FETCH  "y", MULT, STORE "y",PUSH 1, FETCH  "x", SUB, STORE "x"]]

testStorage :: Storage
testStorage "x" = 3

q1 = map (run testCode testStorage) ["x","y"] -- = [1,6]
q2 = testCode == cs testStatement -- = True