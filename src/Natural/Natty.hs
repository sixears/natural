{-# LANGUAGE UnicodeSyntax #-}
module Natural.Natty
  ( Four
  , Natty(Zy, Sy)
  , None
  , One
  , Three
  , Two
  , four
  , none
  , one
  , three
  , two
  ) where

import Base0T

-- more-unicode ------------------------

import Data.MoreUnicode.Bool      ( pattern 𝓣 )
import Data.MoreUnicode.Semigroup ( (◇) )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import Natural.Countable ( Countable(count) )
import Natural.Nat       ( Nat(S, Z) )

--------------------------------------------------------------------------------

data Natty n where Zy :: Natty 'Z
                   Sy :: Natty n -> Natty ('S n)

--------------------

_show ∷ Natty n → String
_show Zy     = "'Z"
_show (Sy n) = "'S " ◇ _show n

instance Show (Natty n) where
  show z = "Natty " ◇ _show z

--------------------

instance Eq (Natty n) where
  Zy     == Zy     = 𝓣
  (Sy n) == (Sy m) = n == m

--------------------

instance Ord (Natty n) where
  Zy     <= _      = 𝓣
  (Sy n) <= (Sy m) = n <= m

--------------------

instance Countable (Natty n) where
  count Zy     = 0
  count (Sy n) = 1 + count n

------------------------------------------------------------

type None  = 'Z
type One   = 'S None
type Two   = 'S One -- ('S 'Z)
type Three = 'S Two
type Four  = 'S Three

none ∷ Natty None -- 'Z
none = Zy

one ∷ Natty One -- ('S 'Z)
one  = Sy none

two ∷ Natty Two -- ('S ('S 'Z))
two  = Sy one

three ∷ Natty Three -- ('S ('S ('S 'Z)))
three  = Sy two

four ∷ Natty Four -- ('S ('S ('S ('S 'Z))))
four  = Sy three

-- that's all, folks! ----------------------------------------------------------
