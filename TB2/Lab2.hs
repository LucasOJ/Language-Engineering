data Tree a = Bra a (Tree a) (Tree a) | Lf 
    deriving (Show)

linear :: Int -> Tree Int
linear 0 = Lf
linear n = Bra n Lf (linear (n - 1))

inorderSlow :: Tree a -> [a]
inorderSlow Lf          = []
inorderSlow (Bra k l r) = (inorderSlow l) ++ k:(inorderSlow r)

ino :: Tree a -> [a] -> [a]
ino Lf acc          = acc
ino (Bra k l r) acc = ino l (k:(ino r acc))

inorder :: Tree a -> [a]
inorder tree = ino tree []

postorderSlow :: Tree a -> [a]
postorderSlow Lf          = []
postorderSlow (Bra k l r) = (postorderSlow l) ++ (postorderSlow r) ++ [k]

post :: Tree a -> [a] -> [a]
post Lf acc          = acc
post (Bra k l r) acc = post l (post r (k:acc))

postorder :: Tree a -> [a]
postorder tree = post tree []