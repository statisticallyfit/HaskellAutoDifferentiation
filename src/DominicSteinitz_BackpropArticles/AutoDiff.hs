{-# LANGUAGE NoMonomorphismRestriction #-}

module DominicSteinitz_BackpropArticles.AutoDiff(Dual(..), idDual, constDual) where
default()

data Dual = Dual Double Double deriving (Eq, Show )


-- Declaring helper functions
constDual :: Double -> Dual
constDual d = Dual d 0

idDual :: Double -> Dual
idDual x = Dual x 1.0


-- Implementing rules of Duals by making Dual instance of Num class
instance Num Dual where
    fromInteger n               = constDual $ fromInteger n
    (Dual x x') + (Dual y y')   = Dual (x + y) (x' + y')
    (Dual x x') * (Dual y y')   = Dual (x * y) (x*y' + y*x')
    negate (Dual x x')          = Dual (negate x) (negate x')
    signum _                    = undefined
    abs _                       = undefined

-- Declaring Dual as instane of Fractional so we can do division
instance Fractional Dual where
    fromRational p = constDual $ fromRational p
    recip (Dual x x') = Dual (1.0 / x) (-x' / (x*x))


-- Need to be able to treat Dual as Float or Double (only instances for Float and
-- Double are defined for the class Floating)
instance Floating Dual where
    pi                = constDual pi
    exp (Dual x x')   = Dual (exp x) (x' * exp x)
    log (Dual x x')   = Dual (log x) (x' / x)
    sqrt  (Dual x x') = Dual (sqrt x)  (x' / (2 * sqrt x))
    sin   (Dual x x') = Dual (sin x)   (x' * cos x)
    cos   (Dual x x') = Dual (cos x)   (x' * (- sin x))
    sinh  (Dual x x') = Dual (sinh x)  (x' * cosh x)
    cosh  (Dual x x') = Dual (cosh x)  (x' * sinh x)
    asin  (Dual x x') = Dual (asin x)  (x' / sqrt (1 - x*x))
    acos  (Dual x x') = Dual (acos x)  (x' / (-sqrt (1 - x*x)))
    atan  (Dual x x') = Dual (atan x)  (x' / (1 + x*x))
    asinh (Dual x x') = Dual (asinh x) (x' / sqrt (1 + x*x))
    acosh (Dual x x') = Dual (acosh x) (x' / sqrt (x*x - 1))
    atanh (Dual x x') = Dual (atanh x) (x' / (1 - x*x))
