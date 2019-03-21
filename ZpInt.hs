module ZpInt where

import Data.List
import Data.Maybe
-- MODULUS MUST BE PRIME!!
data ZpInt = ZpInt {value :: Int, modulus :: Int} deriving(Eq) -- ZpInt i1 i2 = i1 (mod i2)

instance Num ZpInt where
    -- Add two ZpInts
    i1 + i2 
        -- Cannot add different modulus ZpInts
        | (modulus i1) /= (modulus i2) = error ("Cannot add incompatible ZpInts: modulus does not match.")
        -- Cannot have modulus 0
        | (modulus i1 == 0) || (modulus i2 == 0) = error ("Cannot add ZpInt with 0 modulus")
        -- Add values and take first modulus
        | otherwise = ZpInt (((value i1) + (value i2)) `mod` (modulus i1)) (modulus i1)

    i1 * i2
        -- Cannot multiply different modulus ZpInts
        | (modulus i1) /= (modulus i2) = error ("Cannot multiply incompatible ZpInts: modulus does not match.")
        -- Cannot have modulus 0
        | (modulus i1 == 0) || (modulus i2 == 0) = error ("Cannot add ZpInt with 0 modulus")
        -- Multiply values and take first modulus
        | otherwise = ZpInt (((value i1) * (value i2)) `mod` (modulus i1)) (modulus i1)

    -- Return the positive value for a given modulus. This is not abs (value)!
    abs i = let
        v = value i
        m = modulus i
        in
        if v < 0 then ZpInt ((v+m) `mod` m) m else ZpInt (v `mod` m) m

    negate i = abs $ ZpInt (-(value i)) (modulus i) 

    -- Return a ZpInt of -1, 0, or 1.
    signum i = ZpInt (signum $ value $ i) (modulus i)

    -- Not implemented
    fromInteger i = error ("Cannot convert from integer. Please use ZpInt (fromInteger int) (modulus).")

-- Displays ZpInt as "value (mod modulus)"
instance Show ZpInt where
    show i = let 
        reduced = reduce i
        in
        (show $ value $ reduced) ++ " (mod " ++ (show $ modulus reduced) ++ ")"


-- a `divide` b = the element of Zp which when multiplied by b equals a
infix 7 `divide`
divide :: ZpInt -> ZpInt -> ZpInt
divide i1 i2 = let
    v1 = value i1
    v2 = value i2
    m1 = modulus i1
    m2 = modulus i2
    divisor = fromMaybe (ZpInt (-1) (-1)) $ find (\z -> (z*i2)==i1) [ZpInt i m1 | i <- [0..m1-1]]
    in
    if v2 == 0
        then error ("Cannot divide ZpInt by zero!")
    else if m1 /= m2
        then error ("Cannot divide incompatible ZpInts: modulus does not match.")
    else if divisor == (ZpInt (-1) (-1))
        then error ("Cannot divide -- are you working in a field (in prime modulus)?")
    else
        divisor

-- Takes value modulo the modulus
reduce :: ZpInt -> ZpInt
reduce i = let
    v = value i
    m = modulus i
    in
    ZpInt (v `mod` m) m