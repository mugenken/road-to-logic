
-- 1.9
max' :: [Integer] -> Integer
max' []  = error "empty list"
max' [n] = n
max' (n:ns) = max n (max' ns)

-- 1.10
rmFst :: Ord a => a -> [a] -> [a]
rmFst _ [] = []
rmFst r [n] =
    if n == r
        then []
        else [n]
rmFst r (ns) = remove [] ns r
    where remove :: Ord a => [a] -> [a] -> a -> [a]
          remove done [] _ = done
          remove done (n:ns) r =
            if n == r
                then (done ++ ns)
                else remove (done ++ [n]) ns r

-- 1.13
ocount :: Char -> String -> Integer
ocount _ [] = 0
ocount c (x:xs) =
    if c == x
        then 1 + ocount c xs
        else ocount c xs

-- 1.14
blowup :: String -> String
blowup [] = []
blowup (x:xs) = [x] ++ replblow 2 xs
    where replblow :: Integer -> String -> String
          replblow _ [] = []
          replblow n (c:cs) = (replicate' n c) ++ replblow (n + 1) cs
          replicate' :: Integer -> Char -> String
          replicate' n c | n <= 0    = []
                         | otherwise = c : (replicate' (n - 1) c)

-- 1.15
sortStrings :: [String] -> [String]
sortStrings [] = []
sortStrings xs = (min' xs) : (sortStrings $ rmFst (min' xs) xs)

-- 1.17
substring' :: String -> String -> Bool
substring' [] _ = True
substring' _ [] = False
substring' xs (y:ys) = prefix xs (y:ys) || substring' xs ys

-- misc
min' :: Ord a => [a] -> a
min' []  = error "empty list"
min' [n] = n
min' (n:ns) = min n (min' ns)

sortInts :: [Integer] -> [Integer]
sortInts [] = []
sortInts ns = m : (sortInts $ rmFst m ns)
    where m = min' ns

sort' :: Ord a => [a] -> [a]
sort' [] = []
sort' xs = m : (sort' $ rmFst m xs)
    where m = min' xs

length' :: [a] -> Integer
length' [] = 0
length' (_:xs) = 1 + length' xs

sum' :: [Integer] -> Integer
sum' [] = 0
sum' (x:xs) = x + sum' xs

average :: [Integer] -> Rational
average [] = error "empty list"
average xs = toRational (sum' xs) / toRational(length' xs)

prefix :: String -> String -> Bool
prefix [] _ = True
prefix _ [] = False
prefix (x:xs) (y:ys) = x == y && prefix xs ys


