{-# LANGUAGE RankNTypes #-}

module DualNum (DualNum(..), (+~), diff, ndiff) where

import Data.Ratio (numerator, denominator)

-- | The ring of dual numbers obtained by abjoining an element e to r
-- such that e /= 0 and e^2 = 0.
--
-- DualNum u u' == u +~ u' == u + e*u'
--
-- We provide several numerical typeclass instances for Dual. A function f :: r -> r
-- can be extended to a function f' : DualNum r -> DualNum r, which is defined for all
-- (u +~ u') such that f is differentiable at u.
data DualNum r = DualNum { realPart :: r -- | Real part
                   ,  infPart :: r -- | Infinitessimal part
                   } deriving (Eq)
instance Show r => Show (DualNum r) where
  show (DualNum a b) = concat ["(", show a, " +~ ", show b, ")"]

-- | Alternative constructor for DualNum numbers
-- u +~ u' = DualNum u u'
(+~) = DualNum

instance Num a => Num (DualNum a) where
  DualNum u u' + DualNum v v' = (u + v) +~ (u' + v')
  DualNum u u' * DualNum v v' = (u*v) +~ (u'*v + u*v')
  negate (DualNum u u') = negate u +~ negate u'
  abs (DualNum u u') = abs u +~ (u' * signum u)
  signum (DualNum u u') = signum u +~ 0
  fromInteger n = fromInteger n +~ 0

instance Fractional a => Fractional (DualNum a) where
  fromRational r = (fromInteger (numerator r) / fromInteger (denominator r)) +~ 0
  recip (DualNum u u') = recip u +~ (-u' / (u * u))

instance Floating a => Floating (DualNum a) where
  pi = pi +~ 0
  exp (DualNum u u') = exp u +~ (u' * exp u)
  log (DualNum u u') = log u +~ (u' / u)
  sin (DualNum u u') = sin u +~ (u' * cos u)
  cos (DualNum u u') = cos u +~ (-u' * sin u)
  asin (DualNum u u') = asin u +~ (u' / sqrt (1 - u * u))
  acos (DualNum u u') = acos u +~ (-u' / sqrt (1 - u * u))
  atan (DualNum u u') = atan u +~ (u' / (1 + u*u))
  sinh d = (exp d + exp (-d)) / 2
  cosh d = (exp d - exp (-d)) / 2
  asinh (DualNum u u') = asinh u +~ (u' / sqrt (1 + u*u))
  acosh (DualNum u u') = acosh u +~ (u' / sqrt (u*u - 1))
  atanh (DualNum u u') = atanh u +~ (u' / (1 - u*u))

ndiff :: Floating a => Int -> (forall b. Floating b => DualNum b -> DualNum b) -> a -> a
ndiff 0 f x = realPart $ f (x +~ 0)
ndiff 1 f x = infPart $ f (x +~ 1)
ndiff n f x = infPart $ ndiff (n-1) f (x +~ 1)

diff = ndiff 1
