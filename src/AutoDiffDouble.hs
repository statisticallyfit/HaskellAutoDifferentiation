module AutoDiffDouble where

-- AutoDiff.hs
-- Copyright 2015, Daniel Brice

data Dual = Dual Double Double deriving (Read, Show, Eq)

constantDual :: Double -> Dual
-- ^ Lifts a constant number to the `Dual` type.
constantDual x = Dual x 0

seedDual :: Double -> Double -> Dual
-- ^ Creates a Dual number.
seedDual x x' = Dual x x'

evaluateDual :: Dual -> Double
-- ^ Used to evaluate a function.
evaluateDual (Dual x _) = x

differentiateDual :: Dual -> Double
-- ^ Used to evaluate the derivative of a function.
differentiateDual (Dual _ x') = x'


instance Num Dual where
    (+) (Dual u u') (Dual v v') = Dual (u + v) (u' + v')
    (*) (Dual u u') (Dual v v') = Dual (u * v) (u' * v + u * v')
    (-) (Dual u u') (Dual v v') = Dual (u - v) (u' - v')
    negate (Dual u u')          = Dual (negate u) (negate u')
    abs (Dual u u')             = Dual (abs u) (u' * (signum u))
    signum (Dual u u')          = Dual (signum u) 0
    fromInteger n               = Dual (fromInteger n) 0

instance Fractional Dual where
    (/) (Dual u u') (Dual v v') = Dual (u / v) ((u' * v - u * v') / v ** 2)
    recip (Dual u u')           = Dual (recip u) (-1 * u' * (recip (u ** 2)))
    fromRational n              = Dual (fromRational n) 0


instance Floating Dual where
    pi                = Dual pi 0
    exp (Dual u u')   = Dual (exp u) (u' * exp u)
    sqrt (Dual u u')  = Dual (sqrt u) (u' / (2 * sqrt u))
    log (Dual u u')   = Dual (log u) (u' / u)
    sin (Dual u u')   = Dual (sin u) (u' * cos u)
    cos (Dual u u')   = Dual (cos u) (- u' * sin u)
    tan (Dual u u')   = Dual (tan u) (1 / ((cos u) ** 2))
    asin (Dual u u')  = Dual (asin u) (u' / (sqrt(1 - u ** 2)))
    acos (Dual u u')  = Dual (acos u) (- u' / (sqrt(1 - u ** 2)))
    atan (Dual u u')  = Dual (atan u) (u' / (1 + u ** 2))
    sinh (Dual u u')  = Dual (sinh u) (u' * cosh u)
    cosh (Dual u u')  = Dual (cosh u) (u' * sinh u)
    tanh (Dual u u')  = Dual (tanh u) (u' * (1 - (tanh u) ** 2))
    asinh (Dual u u') = Dual (asinh u) (u' / (sqrt(1 + u ** 2)))
    acosh (Dual u u') = Dual (acosh u) (u' / (sqrt(u ** 2 - 1)))
    atanh (Dual u u') = Dual (atanh u) (u' / (1 - u ** 2))
    (**) (Dual u u') (Dual v v')
        = Dual (u ** v) (u ** v * (v' * (log u) + (v * u' / u)))
    logBase (Dual u u') (Dual v v')
        = Dual (logBase u v) (((log v) * u' / u - (log u) * v' / v) / ((log u) ** 2))


--f :: Dual -> Dual
--f x = x ** 3 - sin (x ** 2)

--main = do
--  putStrLn "What's the derivative of f(x) = x^2 - sin(x^2) at x = 2?"
--  print . differentiateDual . f $ Dual 2 1