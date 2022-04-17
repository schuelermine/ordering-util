module Data.Ordering (
    -- * Simple functions
    flipOrdering,
    ifEQ, noLT, noGT,
    -- * Reexports from @base@
    Ordering(..)
) where

import Data.Ord

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

-- | Flip an 'Ordering'
flipOrdering :: Ordering -> Ordering
flipOrdering o =
  case o of
    LT -> GT
    EQ -> EQ
    GT -> LT
