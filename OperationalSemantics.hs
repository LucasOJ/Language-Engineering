module OperationalSemantics(
    Var,
    Num,
    Z,
    T,
    State,
    n_val,
    a_val,
    b_val,
    s_ns
)
where

import Prelude hiding (Num)
--import qualified Prelude (Num)

type Num = Integer
type Var = String

type Z = Integer
type T = Bool

type State = Var -> Z

data Aexp = N Num | V Var | Add Aexp Aexp | Sub Aexp Aexp 
    | Mult Aexp Aexp   
    deriving (Show, Eq, Read)
data Bexp = TRUE | FALSE | Neg Bexp | And Bexp Bexp
    | Eq Aexp Aexp | Le Aexp Aexp
    deriving (Show, Eq, Read)

n_val :: Num -> Z
n_val = id

s :: State
s "x" = 1
s "y" = 2
s "z" = 3
s  _  = 0

a :: Aexp
a = Mult (Add (V "x") (V "y")) (Sub (V "z") (N (-1)))

a_val :: Aexp -> State -> Z
a_val (N n) _     = n_val n
a_val (V x) s     = s x
a_val (Add x y) s = (a_val x s) + (a_val y s)
a_val (Sub x y) s = (a_val x s) - (a_val y s)
a_val (Mult x y)s = (a_val x s) * (a_val y s)

b :: Bexp
b = Neg (Eq (Add (V "x") (V "y")) (N 4))

b_val :: Bexp -> State -> T
b_val TRUE      _ = True
b_val FALSE     _ = False
b_val (Neg x)   s = not (b_val x s)
b_val (And x y) s = (b_val x s) && (b_val y s)
b_val (Eq x y)  s = (a_val x s) == (a_val y s)
b_val (Le x y)  s = (a_val x s) <= (a_val y s)

fv_aexp :: Aexp -> [Var]
fv_aexp (V x)      = [x]
fv_aexp (N _)      = []
fv_aexp (Add x y)  = to_set $ (fv_aexp x) ++ (fv_aexp y)
fv_aexp (Sub x y)  = to_set $ (fv_aexp x) ++ (fv_aexp y)
fv_aexp (Mult x y) = to_set $ (fv_aexp x) ++ (fv_aexp y)

to_set :: Eq a => [a] -> [a]
to_set [] = []
to_set (x:xs) | elem x xs = to_set xs
              | otherwise = x:(to_set xs)

subst_map :: (Aexp -> Aexp) -> Aexp -> Aexp
subst_map f (N n)      = N n
subst_map f (V x)      = f (V x)
subst_map f (Add x y)  = Add (subst_map f x) (subst_map f y)
subst_map f (Sub x y)  = Sub (subst_map f x) (subst_map f y)
subst_map f (Mult x y) = Mult (subst_map f x) (subst_map f y)

subst_aexp :: Aexp -> Var -> Aexp -> Aexp
subst_aexp a1 v a2 = subst_map f a1 where
    f :: Aexp -> Aexp
    f (V x) | x == v    = a2
            | otherwise = (V x) 

data Stm  = Ass Var Aexp | Skip | Comp Stm Stm | If Bexp Stm Stm | While Bexp Stm
    deriving (Show, Eq, Read)

p :: Stm
p = Comp (Ass "z" (N 0)) (While (Le (V "y") (V "x")) (Comp (Ass "z" (Add (V "z") (N 1))) (Ass "x" (Sub (V "x") (V "y")))))

update :: State -> Z -> Var -> State
update s i v x | x == v    = i
               | otherwise = s x

s' :: State
s' = update (update s 17 "x") 5 "y"

data Config = Inter Stm State | Final State

ns_stm :: Config -> Config
ns_stm (Inter (Ass v a) s) = Final $ update s (a_val a s) v
ns_stm (Inter (Comp stm1 stm2) s) = Final s'' where
    Final s'  = ns_stm $ Inter stm1 s
    Final s'' = ns_stm $ Inter stm2 s'
ns_stm (Inter (If cond stm1 stm2) s)
    | b_val cond s = ns_stm $ Inter stm1 s
    | otherwise    = ns_stm $ Inter stm2 s
ns_stm (Inter (While cond stm) s)
    | b_val cond s = Final s''
    | otherwise    = Final s
    where
        Final s'  = ns_stm $ Inter stm s
        Final s'' = ns_stm $ Inter (While cond stm) s'
ns_stm (Inter skip s) = Final s

s_ns :: Stm -> State -> State
s_ns stm s = s' where
    Final s' = ns_stm $ Inter stm s