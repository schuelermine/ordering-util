module Data.Ordering (Ordering(..), ifEQ, noLT, noGT, flipOrdering) where

import Data.Ord

ifEQ :: Ordering -> Ordering -> Ordering
ifEQ i o =
  case o of
    EQ -> i
    x -> x

noLT :: Ordering -> Ordering
noLT o =
  case o of
    LT -> EQ
    x -> x

noGT :: Ordering -> Ordering
noGT o =
  case o of
    GT -> EQ
    x -> x

flipOrdering :: Ordering -> Ordering
flipOrdering o =
  case o of
    LT -> GT
    EQ -> EQ
    GT -> LT
