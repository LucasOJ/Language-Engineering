
{- Evaluation, checking, and compilation of object language expressions -}
{- Stack machines for expression evaluation                             -} 

{- Object language expressions with variable bindings and nested scope  -}

{-# LANGUAGE
     DeriveFunctor #-}

module Intcomp1 where

import Prelude hiding (lookup)
import Control.Applicative 
import Control.Monad.Trans.Class
import Control.Monad.Trans.State
import Data.List (intercalate)


{-*----------------------------------------------------------------------*-}

{- # Expressions with Let-Bindings and Static Scope # -}

data Expr = CstI Int
          | Var  String
          | Let  String Expr Expr
          | Prim String Expr Expr
          | SeqLet [(String, Expr)] Expr
          | SimLet [(String, Expr)] Expr
          deriving Show

eval :: Expr -> [(String, Int)] -> Int 
eval (CstI i) env   = i
eval (Var x)  env   = lookup env x
eval (Let x erhs ebody) env
    = let xval = eval erhs env 
          env1 = ((x, xval):env)
      in  eval ebody env1
eval (Prim op e1 e2) env 
    | op == "+" = eval e1 env + eval e2 env 
    | op == "*" = eval e1 env * eval e2 env
    | op == "-" = eval e1 env - eval e2 env
    | otherwise = error "unknown primitive"
eval (SeqLet [] ebody) env = eval ebody env
eval (SeqLet ((x, expr):xs) ebody) env =
    let xval  = eval expr env
        newenv = updateOrAdd (x, xval) env
    in eval (SeqLet xs ebody) newenv
eval (SimLet xs ebody) env = eval ebody newenv where
    newenv = updateOrAddMany newvals env
    newvals = mapOverSnd (\expr -> eval expr env) xs

{-
eval (SimLet xs ebody) env = eval (SeqLet newsub ebody) env where
    newsub = mapOverSnd (\expr -> eval expr env) xs
-}

updateOrAddMany :: [(String, Int)] -> [(String, Int)] -> [(String, Int)]
updateOrAddMany [] ys = ys
updateOrAddMany (x:xs) ys = updateOrAddMany xs (updateOrAdd x ys)

updateOrAdd :: (String, Int) -> [(String, Int)] -> [(String, Int)]
updateOrAdd x [] = [x]
updateOrAdd (var, newval) ((x, y):xs) | x == var = (var, newval):xs
                                      | otherwise = (x, y):(updateOrAdd (var, newval) xs)

mapOverSnd :: (b -> c) -> [(a,b)] -> [(a, c)]
mapOverSnd f xs = map (\(x, y) -> (x, f y)) xs

lookup :: [(String, Int)] -> String -> Int
lookup env x = 
    case env of []         -> error (x ++ " not found")
                ((y, v):r) -> if x == y then v else lookup r x

run :: Expr -> Int
run e = eval e []


{-*----------------------------------------------------------------------*-}

{- # Closedness # -}

-- mem = elem

mem :: String -> [String] -> Bool
mem x vs 
    = case vs of []     -> False
                 (v:vr) -> x == v || mem x vr

-- |  Checking whether an expression is closed. An expression is closed 
--    if no variable occurs free in the expression. The 'vs' is a list 
--    of the bound variables. 

closedin :: Expr -> [String] -> Bool
closedin (CstI i) vs = True
closedin (Var x) vs  = mem x vs 
closedin (Let x erhs ebody) vs 
    = let vs1 = (x:vs)
      in  closedin erhs vs && closedin ebody vs1
closedin (Prim op e1 e2) vs
    = closedin e1 vs && closedin e2 vs

-- | An expression is closed if it is closed in the empty environment 

closed1 e = closedin e []

-- Some closed expressions: 

e1 = Let "z" (CstI 17) (Prim "+" (Var "z") (Var "z"))

e2 = Let "z" (CstI 17) 
          (Prim "+" (Let "z" (CstI 22) (Prim "*" (CstI 100) (Var "z")))
                    (Var "z"))

e3 = Let "z" (Prim "-" (CstI 5) (CstI 4)) 
          (Prim "*" (CstI 100) (Var "z"))

e4 = Prim "+" (Prim "+" (CstI 20) (Let "z" (CstI 17) 
                                       (Prim "+" (Var "z") (CstI 2))))
              (CstI 30)

e5 = Prim "*" (CstI 2) (Let "x" (CstI 3) (Prim "+" (Var "x") (CstI 4)))


{-*----------------------------------------------------------------------*-}

{- # The Set of Free Variables # -}


-- x occurs bound in the body of 'let x = 6 in x + 3'
-- x occurs free in 'let y = 6 in x + 3'
--                  'let y = x in y + 3'
-- x occurs free (the first time) and bound (the second time) in 'let x = x + 6 in x + 3'


-- | union(xs, ys) is the set of all elements in xs or ys, without duplicates

union :: ([String], [String]) -> [String]
union (xs, ys) 
    = case xs of []     -> ys 
                 (x:xr) -> if mem x ys 
                           then union (xr, ys) 
                           else (x:(union (xr, ys)))

-- | minus(xs, ys) is the set of all elements in xs but not in ys          

minus :: ([String], [String]) -> [String]
minus (xs, ys)
    = case xs of []     -> []
                 (x:xr) -> if mem x ys 
                           then minus (xr, ys)
                           else (x:(minus (xr, ys)))

-- | Find all variables that occur free in expression e

freevars :: Expr -> [String]
freevars (CstI i) = []
freevars (Var x)  = [x]
freevars (Let x erhs ebody) 
    = union (freevars erhs, minus (freevars ebody, [x]))
freevars (Prim op e1 e2) 
    = union (freevars e1, freevars e2)
freevars (SeqLet [] ebody) = freevars ebody
freevars (SeqLet ((x, expr):xs) ebody) =
    union (freevars expr, minus (freevars (SeqLet xs ebody),[x]))

-- | Alternative definition of closed

closed2 e = (freevars e == [])


{-*----------------------------------------------------------------------*-}

{- # Substitution: Replacing Free Variables with Expressions # -}


-- | This version of lookup returns a Var(x) expression if there is no
--   pair (x,e) in the list env --- instead of failing with exception: 

lookOrSelf :: [(String, Expr)] -> String -> Expr 
lookOrSelf [] x         = Var x 
lookOrSelf ((y, e):r) x = if x == y then e else lookOrSelf r x

-- | Remove (x, _) from env: 

remove :: [(String, Expr)] -> String -> [(String, Expr)]
remove [] x         = []
remove ((y, e):r) x = if x == y then r else ((y, e):(remove r x))


-- | Naive Substitution (may capture free variables) 

nsubst :: Expr -> [(String, Expr)] -> Expr 
nsubst (CstI i) env = CstI i
nsubst (Var x)  env = lookOrSelf env x 
nsubst (Let x erhs ebody) env
    = let newenv = remove env x 
      in  Let x (nsubst erhs env) (nsubst ebody newenv)
nsubst (Prim op e1 e2) env
    = Prim op (nsubst e1 env) (nsubst e2 env)

-- Some expressions with free variables 
e6   = Prim "+" (Var "y") (Var "z")

e6s1 = nsubst e6 [("z", CstI 17)]

e6s2 = nsubst e6 [("z", Prim "-" (CstI 5) (CstI 4))]

e6s3 = nsubst e6 [("z", Prim "+" (Var "z") (Var "z"))]

-- Shows that only z outside the Let gets substituted
e7   = Prim "+" (Let "z" (CstI 22) (Prim "*" (CstI 5) (Var "z")))
                (Var "z")

e7s1 = nsubst e7 [("z", CstI 100)]

-- Shows that only the z in the Let rhs gets substituted
e8   = Let "z" (Prim "*" (CstI 22) (Var "z")) (Prim "*" (CstI 5) (Var "z"))

e8s1 = nsubst e8 [("z", CstI 100)]

-- Shows (wrong) capture of free variable z under the Let
e9   = Let "z" (CstI 22) (Prim "*" (Var "y") (Var "z"))

e9s1 = nsubst e9 [("y", Var "z")]

e9s2 = nsubst e9 [("z", Prim "-" (CstI 5) (CstI 4))]


-- | Correct Substitution (avoids capturing free variables)

newtype Fresh s a = Fresh (s -> (a, s)) deriving Functor

instance Applicative (Fresh s) where
    pure a = Fresh (\s -> (a, s))
    (<*>) = liftA2 id

instance Monad (Fresh s) where 
    return x = Fresh $ \s -> (x, s)
    (Fresh h) >>= f = Fresh $ \s -> let (a, newState) = h s 
                                        (Fresh g) = f a 
                                    in  g newState

fresh :: Enum x => Fresh x x
fresh = Fresh (\x -> (x, succ x)) 

runFresh :: Fresh Int a -> (a, Int)
runFresh (Fresh f) = f 0

-- If we used the State monad in our implementation for substitution, 
-- this would use env as our state and expr as our value. This gives 
-- the type: 'State [(String, Expr)] Expr'.
-- If we introduced a counter that we also use as a state alongside 
-- env, we could define this type as 'State ([(String, Expr)], Int) Expr'. 
-- However, using this tuple as a state is troublesome, as we do not want 
-- to have to deal with both the env and counter every time one of them is 
-- updated.
-- Instead, we will isolate the counter from the env, by introducing
-- a State monad where the counter is the state and the previous state monad
-- is the value. This gives the type: 'State Int (State [(String, Expr)] Expr)'.
-- When we use a counter as a state however, there is a better monad we
-- can use than the State monad - this is called the Fresh monad, which
-- is exactly the same except it only has one operation which lets us increment
-- the counter. This gives the type: 'Fresh Int (State [(String, Expr)] Expr)'.
-- Having nested monads can be cleaned up by introducing monad transformers.
-- As the inner monad is the State monad, we can use the StateT monad as the
-- outer monad. This gives us the type: 'StateT [(String, Expr)] (Fresh Int) Expr'.

-- subst :: Expr -> StateT [(String, Expr)] (Fresh Int) Expr 
-- subst (CstI i) 
--     = return (CstI i)
-- subst (Var x)
--     = do 
--      env <- get   
--      return (lookOrSelf env x)
-- subst (Let x erhs ebody) 
--     = do 
--      env <- get
--      erhs1 <- subst erhs 
--      counter <- lift $ fresh
--      let newx   = x ++ show counter
--          newenv = ((x, Var newx):(remove env x)) 
--      put newenv 
--      ebody1 <- subst ebody
--      put env 
--      return (Let newx erhs1 ebody1)
-- subst (Prim op e1 e2) 
--     = do 
--      env <- get 
--      e1' <- subst e1 
--      put env
--      e2' <- subst e2
--      put env
--      return (Prim op e1' e2')

-- runSubst :: Expr -> [(String, Expr)] -> Expr
-- runSubst expr env = let ((expr', env'), counter) = runFresh $ runStateT (subst expr) env
--                     in  expr'

subst :: Expr -> [(String, Expr)] -> Fresh Int Expr
subst (CstI i) env = return (CstI i)
subst (Var x) env  = return (lookOrSelf env x)
subst (Let x erhs ebody) env 
    = do erhs1   <- subst erhs env  
         counter <- fresh
         let newx   = x ++ show counter 
             newenv = ((x, Var newx):(remove env x))
         ebody1  <- subst ebody newenv 
         return (Let newx erhs1 ebody1)
subst (Prim op e1 e2) env
    = do e1' <- subst e1 env
         e2' <- subst e2 env 
         return (Prim op e1' e2')

runSubst :: Expr -> [(String, Expr)] -> Expr
runSubst expr env = let (expr', counter) = runFresh (subst expr env) 
                    in  expr'

-- Shows renaming of bound variable z (to z0)
subst_e7 = runSubst e7 [("z", CstI 100)] 

-- Shows renaming of bound variable z (to z0)
subst_e8 = runSubst e8 [("z", CstI 100)]

-- Shows renaming of bound variable z (to z0), avoiding capture of free z
subst_e9 = runSubst e9 [("y", Var "z")]

{-*----------------------------------------------------------------------*-}

-- | Compilation to target expressions with numerical indexes instead of
--   symbolic variable names.

data TExpr = TCstI Int 
           | TVar Int 
           | TLet TExpr TExpr
           | TPrim String TExpr TExpr
           deriving Show 

-- | Map variable name to variable index at compile-time

getindex :: Eq a => [a] -> a -> Int
getindex [] x     = error "Variable not found"
getindex (y:yr) x = if x == y then 0 else 1 + getindex yr x

-- | Compiling from Expr to TExpr

tcomp :: Expr -> [String] -> TExpr 
tcomp (CstI i) cenv = TCstI i
tcomp (Var x) cenv = TVar (getindex cenv x)
tcomp (Let x erhs ebody) cenv
    = let cenv1 = (x:cenv)
      in  TLet (tcomp erhs cenv) (tcomp ebody cenv1)
tcomp (Prim op e1 e2) cenv
    = TPrim op (tcomp e1 cenv) (tcomp e2 cenv)
tcomp (SeqLet [] ebody) cenv = tcomp ebody cenv
tcomp (SeqLet ((x, expr):xs) ebody) cenv =
    let cenv1 = (x:cenv)
    in TLet (tcomp expr cenv) (tcomp (SeqLet xs ebody) cenv1)

-- | Evaluation of target expressions with variable indexes. The
--   run-time environment renv is a list of variable values (ints).

teval :: TExpr -> [Int] -> Int 
teval (TCstI i) renv   = i
teval (TVar n)  renv   = renv !! n
teval (TLet erhs ebody) renv
    = let xval  = teval erhs renv 
          renv1 = (xval:renv)
      in  teval ebody renv1
teval (TPrim op e1 e2) renv 
    | op == "+" = teval e1 renv + teval e2 renv 
    | op == "*" = teval e1 renv * teval e2 renv
    | op == "-" = teval e1 renv - teval e2 renv
    | otherwise = error "unknown primitive"

-- Correctness: eval e []  equals  teval (tcomp e []) [] 


{-*----------------------------------------------------------------------*-}

{- # Stack Machines # -}

-- | Stack machine instructions. An expressions in postfix or reverse
--   Polish form is a list of stack machine instructions.

data RInstr = RCstI Int 
            | RAdd 
            | RSub 
            | RMul
            | RDup
            | RSwap
            deriving Show 

-- | A simple stack machine for evaluation of variable-free expressions
--   in postfix form

reval :: [RInstr] -> [Int] -> Int 
reval [] (v:vs)                 = v 
reval [] []                     = error "reval: no result on stack!"
reval ((RCstI i):insr) stk      = reval insr (i:stk)
reval (RAdd:insr) (i2:i1:stkr)  = reval insr ((i1+i2):stkr)
reval (RSub:insr) (i2:i1:stkr)  = reval insr ((i1-i2):stkr)
reval (RMul:insr) (i2:i1:stkr)  = reval insr ((i1*i2):stkr)
reval (RDup:insr) (i1:stkr)     = reval insr (i1:i1:stkr)
reval (RSwap:insr) (i2:i1:stkr) = reval insr (i1:i2:stkr)
reval _ _                       = error "reval: too few operands on stack"
        
-- | Compilation of a variable-free expression to a RInstr list

rcomp :: Expr -> [RInstr]
rcomp (CstI i)        = [RCstI i]
rcomp (Var _)         = error "rcomp cannot compile Var"
rcomp (Let _ _ _)     = error "rcomp cannot compile Let"
rcomp (Prim op e1 e2) =
    case op of 
        "+" -> rcomp e1 ++ rcomp e2 ++ [RAdd]
        "-" -> rcomp e1 ++ rcomp e2 ++ [RSub]
        "*" -> rcomp e1 ++ rcomp e2 ++ [RMul]
        _   -> error "unknown primitive"

-- Correctness: eval e []  equals  reval (rcomp e) [] 


-- | Storing intermediate results and variable bindings in the same stack 

data SInstr = SCstI Int     -- push integer
            | SVar Int      -- push variable from env
            | SAdd          -- pop args, push sum
            | SSub          -- pop args, push difference
            | SMul          -- pop args, push product
            | SPop          -- pop value/unbind var
            | SSwap         -- exchange top and next
            deriving (Show)


seval :: [SInstr] -> [Int] -> Int 
seval [] (v:vs)                 = v 
seval [] []                     = error "seval: no result on stack!"
seval ((SCstI i):insr) stk      = seval insr (i:stk)
seval ((SVar i):insr)  stk      = seval insr ((stk !! i):stk)
seval (SAdd:insr)  (i2:i1:stkr) = seval insr ((i1+i2):stkr)
seval (SSub:insr)  (i2:i1:stkr) = seval insr ((i1-i2):stkr)
seval (SMul:insr)  (i2:i1:stkr) = seval insr ((i1*i2):stkr)
seval (SPop:insr)  (_:stkr)     = seval insr stkr
seval (SSwap:insr) (i2:i1:stkr) = seval insr (i1:i2:stkr)
seval _ _                       = error "seval: too few operands on stack"
        

-- | A compile-time variable environment representing the state of
--   the run-time stack.

data StackValue = Value         -- a computed value
                | Bound String  -- a bound variable
                deriving Eq

-- | Compilation to a list of instructions for a unified-stack machine

scomp :: Expr -> [StackValue] -> [SInstr]
scomp (CstI i) cenv = [SCstI i]
scomp (Var x) cenv  = [SVar (getindex cenv (Bound x))]
scomp (Let x erhs ebody) cenv
    = scomp erhs cenv ++ scomp ebody ((Bound x):cenv) ++ [SSwap, SPop]
scomp (Prim op e1 e2) cenv
    = case op of 
        "+" -> scomp e1 cenv ++ scomp e2 (Value:cenv) ++ [SAdd] 
        "-" -> scomp e1 cenv ++ scomp e2 (Value:cenv) ++ [SSub] 
        "*" -> scomp e1 cenv ++ scomp e2 (Value:cenv) ++ [SMul] 
        _   -> error "scomp: unknown operator"


s1 = scomp e1 []
s2 = scomp e2 []
s3 = scomp e3 []
s5 = scomp e5 []

-- | Output the integers in list inss to the text file called fname: 

intsToFile :: [Int] -> String -> IO ()
intsToFile inss fname = do 
                let text = intercalate " " (map show inss)
                writeFile fname text

assemble :: [SInstr] -> [Int]
assemble [] = []
assemble (x:xs) 
 = let xs' = assemble xs 
    in case x of SCstI i -> (0:i:xs')
                 SVar  i -> (1:i:xs')
                 SAdd    -> (2:xs')
                 SSub    -> (3:xs')
                 SMul    -> (4:xs')
                 SPop    -> (5:xs')
                 SSwap   -> (6:xs')

sbyte :: Expr -> [StackValue] -> [Int] 
sbyte expr = assemble . (scomp expr)

scompeval :: Expr -> [StackValue] -> IO ()
scompeval x = flip intsToFile "fname" . assemble . scomp x