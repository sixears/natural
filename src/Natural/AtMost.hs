{-# LANGUAGE UnicodeSyntax #-}
module Natural.AtMost
  ( AtMost(Nil, Cons)
  , atMost
  , atMostOne
  , atMostTwo
  , zeroOneOrTwo
  ) where

import Base0T

-- base --------------------------------

import Control.Applicative ( Alternative )

-- more-unicode ------------------------

import Data.MoreUnicode.Applicative ( (∤), (⊵) )
import Data.MoreUnicode.Functor     ( (⊳) )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import Natural.Countable ( Countable(count) )
import Natural.Nat       ( Nat(S) )
import Natural.Natty     ( Natty(Sy, Zy), One, Two, one, two )

--------------------------------------------------------------------------------

data AtMost n a where Nil :: AtMost n a
                      Cons :: a -> AtMost n a -> AtMost ('S n) a

--------------------

instance Eq (AtMost n a) where
  a == b = (count a) == (count b)

--------------------

instance Ord (AtMost n a) where
  a <= b = (count a) <= (count b)

--------------------

instance Countable (AtMost n a) where
  count Nil        = 0
  count (Cons _ x) = 1 + count x

------------------------------------------------------------

atMost ∷ Alternative f ⇒ Natty n → f a → f (AtMost n a)
atMost Zy _     = pure Nil
atMost (Sy n) a = (Cons ⊳ a ⊵ atMost n a) ∤ pure Nil

----------------------------------------

{-# DEPRECATED atMostOne "use `atMost one` instead" #-}
atMostOne ∷ Alternative f ⇒ f a → f (AtMost One a)
atMostOne = atMost one

----------------------------------------

{-# DEPRECATED atMostTwo "use `atMost two` instead" #-}
atMostTwo ∷ Alternative f ⇒ f a → f (AtMost Two a)
atMostTwo = atMost two

----------------------------------------

zeroOneOrTwo ∷ Alternative f ⇒ f a → f [a]
zeroOneOrTwo a = go (2 :: ℕ)
  where
    go n
      | n > 0 = ((:) ⊳ a ⊵ go (n - 1)) ∤ pure []
      | otherwise = pure []

-- that's all, folks! ----------------------------------------------------------
