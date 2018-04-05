{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Modular (
    Mod(..),
    toMod,
    unMod,
    modulus,
    inv
) where

import GHC.TypeLits 
import Data.Proxy

-- | Our Mod type 
--    n is the number. This is a Haskell runtime value
--    m is the modulus. This is a type variable
newtype Mod n (m :: Nat) = Mod n
    deriving (Show, Eq)

-- | Create a Number modulo m.
-- Note the modulus m must be detectable from the type.
-- Either you have to give an explicut type, or it must be infereable
toMod :: forall n m. (Integral n, KnownNat m) => n -> Mod n m 
toMod n = Mod (n `mod` (fromInteger (natVal (Proxy :: Proxy m))))

-- | Get the number out of our Mod type
unMod :: Mod n m -> n 
unMod (Mod n) = n

-- | Extract the modulus out of the type
modulus :: forall n m. (Integral n, KnownNat m) => Mod n m -> n
modulus i = fromInteger $ natVal i

-- Instances
instance (Integral n, KnownNat m) => Num (Mod n m) where
    fromInteger = toMod . fromInteger
    (Mod n1) + (Mod n2) = toMod (n1 + n2)
    (Mod n1) - (Mod n2) = toMod (n1 - n2)
    (Mod n1) * (Mod n2) = toMod (n1 * n2)
    abs = toMod . abs . unMod 
    signum = toMod . signum . unMod

instance (Integral n, KnownNat m) => Bounded (Mod n m) where
    minBound = 0 
    maxBound = toMod ( ( fromInteger $ natVal (Proxy :: Proxy m)) - 1)

instance (Integral n, KnownNat m) => Enum (Mod n m) where
    toEnum   = fromInteger . toInteger
    fromEnum = fromInteger . toInteger . unMod
    enumFrom     x   = enumFromTo     x maxBound
    enumFromThen x y = enumFromThenTo x y bound
      where
        bound | fromEnum y >= fromEnum x = maxBound
              | otherwise                = minBound
  
-- Extended Euclidean algorithm.  Given non-negative a and b, return x, y and g
-- such that ax + by = g, where g = gcd(a,b).  Note that x or y may be negative.
-- From https://rosettacode.org/wiki/Modular_inverse
gcdExt :: Integral a => a -> a -> (a, a, a)
gcdExt a 0 = (1, 0, a)
gcdExt a b = let (q, r) = a `quotRem` b
                 (s, t, g) = gcdExt b r
             in (t, s - q * t, g)
 
-- Given a and m, return Just x such that ax = 1 mod m.  
-- If there is no such x return Nothing.
modInv :: Integral a => a -> a -> Maybe a
modInv a m = let (i, _, g) = gcdExt a m
             in if g == 1 then Just (mkPos i) else Nothing
  where mkPos x = if x < 0 then x + m else x

-- Return the inverse 
inv :: (Integral i, KnownNat n) => Mod i n -> Maybe (Mod i n)
inv x = toMod <$> modInv (unMod x) (modulus x)