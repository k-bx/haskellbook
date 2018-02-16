{-# LANGUAGE ScopedTypeVariables #-}

f :: Eq a => [a] -> Bool
f = all =<< (==) . head

-- f :: Eq a => [a] -> Bool
-- f xs = (all =<< (==) . head) xs

-- f :: Eq a => [a] -> Bool
-- f xs = (all =<< ((==) . head)) xs

-- f :: Eq a => [a] -> Bool
-- f xs = (do
--            y <- ((==) . head) :: Eq a0 => [a0] -> a0 -> Bool
--            all y) xs

-- f :: Eq a => [a] -> Bool
-- f xs = ((y <- ((==) . head) :: Eq a0 => [a0] -> a0 -> Bool)
--         `monadOpFunc`
--         (all y)) xs

-- instance Monad ((->) r) where
--     f >>= k = \ r -> k (f r) r
