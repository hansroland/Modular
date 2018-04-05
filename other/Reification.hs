{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Reification where 

import Modular
import GHC.TypeLits
import Data.Proxy

-- Try to do something with reification 
main :: IO ()
main = do 
    -- Read the values from the user
    putStrLn "Enter the modulus"
    imod :: Integer <- readLn 
    putStrLn "Enter first number"
    n1 :: Integer <- readLn 
    putStrLn "Enter second number"
    n2 :: Integer <- readLn 
    -- Create the types
    let Just dynNat = someNatVal imod
    case dynNat of
       SomeNat (_ :: Proxy m) -> do
           let mm1, mm2  :: Mod Integer m
               mm1 = toMod n1 
               mm2 = toMod n2 
           print $ mm1 + mm2
-- Note: This can be done without type level hackery. It does not give any real benefits
-- TODO: Same with Strings 
-- TODO: Two different values in the type
