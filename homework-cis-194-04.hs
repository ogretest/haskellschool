
-- Exercise 1

newtype Poly a = P [a]

x :: Num a => Poly a
x = P [1]

-- Exercise 2

instance (Eq a) => Eq (Poly a) where
  P cs == P cs' = foldr (\ (a, b) r -> r && a == b) (length cs == length cs') $ zip cs cs'

-- Exercise 3

instance (Show a, Eq a, Num a) => Show (Poly a) where
  show (P cs') = show' $ P (normalizePoly cs')
    where
      show' (P cs)
        | null cs = "error"
        | length cs == 1 = show $ head cs
        | otherwise = snd $ foldl f ((0, False), "") cs
          where
            f ((n, h), p) c
              | c == 0 = ((n', False), showPlus ++ p)
              | otherwise = ((n', True), showC ++ showX ++ showPlus ++ p)
              where
                n' = n + 1
                showPlus = if h then " + " else ""
                showC = if n == 0 then show c else case c of
                  -1 -> "-"
                  1 -> ""
                  _ -> show c
                showX = if n == 0 then "" else "x" ++ "^" ++ show n

-- Exercise 4

instance (Eq a, Num a) => Num (Poly a) where
  (+) (P ls) (P rs) = P (ls `plusPoly` rs)
    where
      (ls', rs') = normalizePolySize ls rs
  (-) (P ls) (P rs) = P (ls `minusPoly` rs)
    where
      (ls', rs') = normalizePolySize ls rs
  (*) (P ls) (P rs) = P (ls `timesPoly` rs)

  fromInteger i = P [fromInteger i]
  abs (P ls) = P (map abs ls)
  signum (P ls) = P [signum $ last ls]

binaryPoly :: (Eq a, Num a) => (a -> a -> a) -> [a] -> [a] -> [a]
binaryPoly f ls rs = normalizePoly (uncurry (zipWith f) $ normalizePolySize ls rs)

plusPoly :: (Eq a, Num a) => [a] -> [a] -> [a]
plusPoly = binaryPoly (+)

minusPoly :: (Eq a, Num a) => [a] -> [a] -> [a]
minusPoly = binaryPoly (-)

plusPoly' ls rs = uncurry (zipWith (+)) $ normalizePolySize ls rs

timesPoly :: (Eq a, Num a) => [a] -> [a] -> [a]
timesPoly ls rs = fst $ foldr f ([], []) ls
  where
    f l (b, i) = (b', i')
      where
        b' = (0 : b) `plusPoly'` foldr f' i rs
          where
            f' r s = l * r : s
        i' = 0 : i

normalizePoly :: (Eq a, Num a) => [a] -> [a]
normalizePoly cs = if all (== 0) cs then [0] else fst $ foldr (\ a (r, p) -> if p && a == 0 then ([], True) else (a : r, False)) ([], True) cs

normalizePolySize :: (Num a) => [a] -> [a] -> ([a], [a])
normalizePolySize ls rs = (ls', rs')
  where
    lengthLeft = length ls
    lengthRight = length rs
    addZero = replicate (abs (lengthLeft - lengthRight)) 0
    ls' = if lengthLeft > lengthRight then ls else ls ++ addZero
    rs' = if lengthLeft < lengthRight then rs else rs ++ addZero

-- Exercise 4

-- Exercise 5

-- Exercise 6

-- Excecise 7

applyP :: (Num a) => Poly a -> a -> a
applyP (P cs) x = foldr (\ c s -> s * x + c) 0 cs

-- Excercise 8

class Num a => Differentiable a where
  deriv :: a -> a
  nderiv :: Int -> a -> a

instance (Eq a, Num a) => Differentiable (Poly a) where
  deriv (P cs) = P (normalizePoly (derivPoly cs))
  nderiv n p@(P cs) = if n == 0 then p else nderiv (n - 1) (deriv p)

derivPoly cs
  | len < 2 = [0]
  | otherwise = fst $ foldr f ([], fromIntegral (len - 1)) (tail cs)
  where
    len = length cs
    -- f :: (Fractional a) => a -> ([a], Int) -> ([a], Int)
    f c (s, n) = (n * c : s, n - 1)
