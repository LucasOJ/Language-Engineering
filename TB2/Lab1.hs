data AExpr = CstI Int
          | Var String
          | Add AExpr AExpr
          | Mul AExpr AExpr
          | Sub AExpr AExpr
          deriving (Eq, Show)
{-
fmt :: AExpr -> String
fmt (CstI x) = show x
fmt (Var x) = x
fmt (Add x y) = "(" ++ (fmt x) ++ " + " ++ (fmt y) ++ ")"
fmt (Mul x y) = "(" ++ (fmt x) ++ " * " ++ (fmt y) ++ ")"
fmt (Sub x y) = "(" ++ (fmt x) ++ " - " ++ (fmt y) ++ ")"
-}
fmt :: AExpr -> Int -> String
fmt (CstI x)  pre = show x
fmt (Var x)   pre = x
fmt (Add x y) pre = (fmt x 1) ++ " + " ++ (fmt x 1)
fmt (Mul x y) pre = (fmt x 1) ++ " * " ++ (fmt x 1)
fmt (Sub x y) pre
    | pre > 5   = "(" ++ (fmt x 5) ++ " - " ++ (fmt y 6) ++ ")"
    | otherwise = (fmt x 5) ++ " - " ++ (fmt x 5)



simplify :: AExpr -> AExpr
simplify (Add (CstI 0) x) = x
simplify (Add x (CstI 0)) = x
simplify (Sub x (CstI 0)) = x 
simplify (Mul (CstI 1) x) = x
simplify (Mul x (CstI 1)) = x
simplify (Mul (CstI 0) x) = CstI 0
simplify (Mul x (CstI 0)) = CstI 0
simplify (Sub x y)
    | x == y    = CstI 0
    | otherwise = Sub x y
simplify x = x

