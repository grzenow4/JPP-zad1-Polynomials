module SparsePoly(fromDP, toDP, qrP) where
import DensePoly(toCanonicalDP)
import PolyClass
import Representation

qsort :: [(Int, a)] -> [(Int, a)]
qsort [] = []
qsort ((x,a):xs) = qsort ys ++ [(x,a)] ++ qsort zs where
    ys = [y | y <- xs, fst y >= x]
    zs = [z | z <- xs, fst z < x]

toCanonicalSP :: (Eq a, Num a) => SparsePoly a -> SparsePoly a
toCanonicalSP = S . filter (\(_, x) -> x /= 0) . qsort . unS

-- | fromDP example
-- >>> fromDP sampleDP
-- S {unS = [(3,1),(0,-1)]}
fromDP :: (Eq a, Num a) => DensePoly a -> SparsePoly a
toDP :: (Eq a, Num a) => SparsePoly a -> DensePoly a

fromDP = toCanonicalSP . S . reverse . convert 0 . unP where
    convert _ [] = []
    convert n (x:xs) = (n, x) : convert (n + 1) xs
toDP (S []) = P []
toDP (S ((n, x):xs)) = toCanonicalDP . P . reverse $ convert ((n, x):xs) n where
    convert [] m = if m < 0 then [] else 0 : convert [] (m - 1)
    convert ((n, x):xs) m
        | n == m = x : convert xs (m - 1)
        | otherwise = 0 : convert ((n, x):xs) (m - 1)

first :: (a -> a') -> (a, b) -> (a', b)
first f (x, y) = (f x, y)
second :: (b -> b') -> (a, b) -> (a, b')
second f (x, y) = (x, f y)

instance Functor SparsePoly where
    fmap f = S . map (second f) . unS

instance Polynomial SparsePoly where
    zeroP = S []
    constP c = toCanonicalSP $ S [(0, c)] 
    varP = S [(1, 1)]
    evalP (S xs) a = foldl (\res (n, x) -> res + x * a ^ n) 0 xs
    shiftP n = toCanonicalSP . S . map (first (+n)) . unS
    degree (S []) = -1
    degree s = fst . head . unS $ toCanonicalSP s

instance (Eq a, Num a) => Num (SparsePoly a) where
    (+) (S xs) (S ys) = toCanonicalSP . S $ helper xs ys where
        helper p [] = p
        helper [] q = q
        helper p@((n, x):xs) q@((m, y):ys)
            | n == m = (n, x + y) : helper xs ys
            | n < m = (m, y) : helper p ys
            | n > m = (n, x) : helper xs q
    (*) (S []) _ = zeroP
    (*) (S [(0, x)]) q = toCanonicalSP $ fmap (*x) q
    (*) (S ((n, x):xs)) q = shiftP n (constP x * q) + S xs * q
    abs = undefined
    signum = undefined
    fromInteger = constP . fromInteger
    negate = toCanonicalSP . fmap (*(-1))

instance (Eq a, Num a) => Eq (SparsePoly a) where
    (==) p q = nullP $ p - q

-- qrP s t | not(nullP t) = (q, r) iff s == q*t + r && degree r < degree t
qrP :: (Eq a, Fractional a) => SparsePoly a -> SparsePoly a -> (SparsePoly a, SparsePoly a)
qrP p s = (q, r) where
    q = S $ helper (toCanonicalSP p) (toCanonicalSP s)

    helper _ (S []) = error "Cannot divide by 0"
    helper (S []) _ = []
    helper p@(S ((n, x):xs)) s@(S ((m, y):ys))
        | n < m = []
        | otherwise = (n - m, x / y) : helper (p - (S [(n - m, x / y)]) * s) s

    r = p - q * s

-- | Division example
-- >>> qrP (x^2 - 1) (x -1) == ((x + 1), 0)
-- True
