-- exercise 2

f1 :: a -> a -> a
f1 a b = a

f2 :: a -> a -> a
f2 a b = b

-- won't compile!
f3 :: a -> a -> a
f3 a b = a + b
