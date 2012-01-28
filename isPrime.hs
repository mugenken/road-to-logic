
devides :: Integer -> Integer -> Bool
devides d n = n `rem` d == 0

ldf :: Integer -> Integer -> Integer
ldf k n | devides k n = k
        | k ^ 2 > n   = n
        | otherwise   = ldf (k + 1) n

ld :: Integer -> Integer
ld n = ldf 2 n

prime0 :: Integer -> Bool
prime0 n | n < 1     = error "invalid integer"
         | n == 1    = False
         | otherwise = ld n == n

