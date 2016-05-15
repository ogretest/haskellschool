myLast :: [a] -> a
myLast [x] = x
myLast (_ : xs) = myLast xs

elementAt :: [a] -> Int -> a
elementAt (x : _) 0 = x
elementAt (_ : xs) n = elementAt xs (n - 1)

isPalindrom :: (Eq a) => [a] -> Bool
isPalindrom [] = True
isPalindrom [x] = True
isPalindrom l = if head l == last l then isPalindrom (init (tail l)) else False

compress :: Eq a => [a] -> [a]
compress [] = []
compress [x] = [x]
compress (x0 : x1 : xs) = if x0 == x1 then compress (x1:xs) else x0 : compress (x1 : xs)

encode :: Eq a => [a] -> [(Int, a)]
encode [] = []
encode (x0:xs) = result
  where
    count n [] = (n, [])
    count n (xn:xs) = if x0 == xn then count (n + 1) xs else (n, xn:xs)
    (n, xs') = count 1 xs
    result = (n, x0) : encode xs'

