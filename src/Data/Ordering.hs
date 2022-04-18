module Data.Ordering (
    -- * Simple functions
    flipOrdering,
    ifEQ, noLT, noGT,
    isLE, isNE, isGE,
    toSign, fromSign, adjustBy,
    -- * Reexports from @base@
    Ordering(..)
) where

import Data.Ord (Ordering(..))

-- | Flip an 'Ordering'
flipOrdering :: Ordering -> Ordering
flipOrdering o =
  case o of
    LT -> GT
    EQ -> EQ
    GT -> LT

-- | Replace the 'EQ' case with another 'Ordering'
ifEQ :: Ordering -> Ordering -> Ordering
ifEQ i o =
  case o of
    EQ -> i
    x -> x

-- | Clamp 'LT' to 'EQ'
noLT :: Ordering -> Ordering
noLT o =
  case o of
    LT -> EQ
    x -> x

-- | Clamp 'GT' to 'EQ'
noGT :: Ordering -> Ordering
noGT o =
  case o of
    GT -> EQ
    x -> x

-- | Check an 'Ordering' represents less-or-equal
isLE :: Ordering -> Bool
isLE = (/= GT)

-- | Check an 'Ordering' represents not-equal
isNE :: Ordering -> Bool
isNE = (/= EQ)

-- | Check an 'Ordering' represents greater-or-equal
isGE :: Ordering -> Bool
isGE = (/= LT)

-- | Returns -1, 0, and 1 for 'LT', 'EQ', and 'GT', respectively
toSign :: Num n => Ordering -> n
toSign o =
  case o of
    LT -> -1
    EQ -> 0
    GT -> 1

-- | Get an 'Ordering' depending on the sign of a number
--
--   Compares the number to 0
fromSign :: (Ord n, Num n) => n -> Ordering
fromSign o = compare o 0

-- | Adjust a number by a difference up, down, or not at all
adjustBy :: Num a => a -> Ordering -> a -> a
adjustBy d o x =
  case o of
    LT -> x - d
    EQ -> x
    GT -> x + d
