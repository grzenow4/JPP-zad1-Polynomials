module DensePoly(toCanonicalDP) where
import PolyClass
import Representation

toCanonicalDP :: (Eq a, Num a) => DensePoly a -> DensePoly a
toCanonicalDP = P . reverse . dropWhile (==0) . reverse . unP

instance Functor DensePoly where
    fmap f = P . map f . unP

instance Polynomial DensePoly where
    zeroP = P []
    constP c = toCanonicalDP . P $ [c]
    varP = P [0,1]
    evalP (P xs) a = fst $ foldl (\(res, n) x -> (res + x * a ^ n, (n + 1))) (0, 0) xs
    shiftP n = toCanonicalDP . P . helper n . unP where
        helper 0 xs = xs
        helper n xs = if n > 0 then 0 : helper (n - 1) xs else xs
    degree = pred . length . unP . toCanonicalDP

instance (Eq a, Num a) => Num (DensePoly a) where
    (+) (P xs) (P ys) = toCanonicalDP . P $ helper xs ys where
        helper xs [] = xs
        helper [] ys = ys
        helper (x:xs) (y:ys) = (x + y) : helper xs ys
    (*) (P [x]) q = toCanonicalDP $ fmap (*x) q
    (*) p q = helper 0 p q where
        helper _ (P []) _ = zeroP
        helper n (P (x:xs)) q = shiftP n (constP x * q) + helper (n + 1) (P xs) q
    abs = undefined
    signum = undefined
    fromInteger = constP . fromInteger
    negate = toCanonicalDP . fmap (*(-1))

-- |
-- >>> x^3 - 1 :: DensePoly Integer 
-- P {unP = [-1,0,0,1]}

-- | Num operations give canonical results:
-- >>> isCanonicalDP (sampleDP - sampleDP)
-- True

instance (Eq a, Num a) => Eq (DensePoly a) where
    (==) p q = nullP $ p - q

-- |
-- >>>  P [1,2] == P [1,2]
-- True

-- |
-- >>> fromInteger 0 == (zeroP :: DensePoly Int)
-- True

-- |
-- >>>  P [0,1] == P [1,0]
-- False

-- | Degree examples
-- >>> degree (zeroP :: DensePoly Int)
-- -1
-- >>> degree (constP 1 :: DensePoly Int)
-- 0
