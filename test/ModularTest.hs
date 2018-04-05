module ModularTest where

import Modular
import Data.Maybe (fromJust)

-- $setup
-- >>> :set -XDataKinds
-- >>> let m1 = Mod 5 :: Mod Int 7
-- >>> let m2 = toMod 4 :: Mod Int 7
-- >>> let m3 = toMod (-1) :: Mod Int 9 

-- $
-- >>> m1 
-- Mod 5
-- >>> m3
-- Mod 8
-- >>> m1 + m2
-- Mod 2
-- >>> m1 - m2
-- Mod 1
-- >>> m1 * m2
-- Mod 6
-- >>> m1 * fromJust (inv m1)
-- Mod 1
-- >>> maxBound :: (Mod Int 7)
-- Mod 6
-- 
