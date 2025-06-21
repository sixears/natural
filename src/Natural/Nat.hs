{-# LANGUAGE UnicodeSyntax #-}
module Natural.Nat
  ( Nat(Z, S)
  ) where

import Base0T

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import Natural.Countable ( Countable(count) )

--------------------------------------------------------------------------------

{- Thank you to dfeuer, redneb, leftroundabout on StackOverflow
   http://stackoverflow.com/questions/39690844/haskell-how-do-i-create-a-function-that-allows-none-one-or-two-applicatives
-}

data Nat = Z
         | S Nat
  deriving (Eq, Ord, Show)

instance Countable Nat where
  count Z     = 0
  count (S n) = 1 + count n

-- that's all, folks! ----------------------------------------------------------
