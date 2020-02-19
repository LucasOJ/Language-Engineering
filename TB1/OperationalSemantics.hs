import BaseTypes

s :: State
s "x" = 1
s "y" = 2
s "z" = 3
s  _  = 0

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

testStatement1 :: Stm
testStatement1 = Comp (Ass "z" (N 0)) (While (Le (V "y") (V "x")) (Comp (Ass "z" (Add (V "z") (N 1))) (Ass "x" (Sub (V "x") (V "y")))))