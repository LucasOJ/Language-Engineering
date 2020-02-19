import BaseTypes

cond :: (a -> T, a -> a, a -> a) -> (a -> a)
cond (b, f1, f2) x | b x       = f1 x
                   | otherwise = f2 x

fix :: ((State -> State) -> (State -> State)) -> (State -> State)
fix ff = ff (fix ff) --returns state transformer

s_ds :: Stm -> (State -> State)
s_ds (Ass v a) s = update s (a_val a s) v
s_ds (Skip) s = s
s_ds (Comp stm1 stm2) s = ((s_ds stm2) . (s_ds stm1)) s
s_ds (If b stm1 stm2) s = cond(b_val b, s_ds stm1, s_ds stm2) s
s_ds (While b stm) s = fix ff s where
    ff :: (State -> State) -> (State -> State)
    ff g = cond (b_val b, g . (s_ds stm), id)

testStatement1 :: Stm
testStatement1 = Comp (Ass "z" (N 0)) (While (Le (V "y") (V "x")) (Comp (Ass "z" (Add (V "z") (N 1))) (Ass "x" (Sub (V "x") (V "y")))))

s'' :: State
s'' "x" = 13
s'' "y" = 5
s''  _  = 0