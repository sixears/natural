{-# LANGUAGE UnicodeSyntax #-}
module Natural.Enum
  ( allEnum
  , fromEnum
  , fromEnum_
  , toEnum
  , toEnum'
  , toEnum_
  ) where

import Base0T

-- base --------------------------------

import GHC.Enum qualified

import Data.Typeable ( typeOf )
import Prelude       ( Bounded(maxBound), Enum, Integral )

-- more-unicode ------------------------

import Data.MoreUnicode.Bool    ( pattern 𝓣 )
import Data.MoreUnicode.Functor ( (⩺) )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import Natural.BoundedError ( AsBoundedError, throwUpperBoundError )
import Natural.Unsigned     ( I64(I64), Unsigned, i64ToInt, ı, ɨ )

--------------------------------------------------------------------------------

fromEnum ∷ ∀ ε α ν η .
           (Unsigned ν,Integral ν,AsBoundedError ε ν,MonadError ε η,Enum α)⇒
           α → η ν
fromEnum = ı ∘ GHC.Enum.fromEnum

--------------------

fromEnum_ ∷ ∀ α ν . (Unsigned ν, Integral ν, Enum α, Show ν) ⇒ α → ν
fromEnum_ = ɨ ∘ GHC.Enum.fromEnum

----------------------------------------

toEnum ∷ ∀ ε ν α η .
         (Bounded α, Enum α, Unsigned ν, Integral ν,
          AsBoundedError ε I64, MonadError ε η)⇒
         ν → η α
toEnum input = do
  result ← (GHC.Enum.toEnum ∘ i64ToInt ⩺ ı) input
  let max_result = maxBound
      max_int = fromEnum_ max_result -- max input value for this enum
  if 𝓣
  then (if fromIntegral input > max_int
        then throwUpperBoundError (typeOf I64) (fromIntegral input) max_int
        else return result)
  else return max_result -- never used, just forces the type for max_result

--------------------

{- | `toEnum`, but for not (upper-)bounded types.  If you use this for
     a type with an upper bound, hilarity and/or sadness may ensue.

     `GHC.Enum.toEnum ∷ Int -> α`; that `Int` is `Int64` in practice…
     so in practice, enums (or at least toEnum) is restricted to `Int64`:
     this is necessarily reflected in the type of `BoundedError`.
-}
toEnum' ∷ ∀ ε ν α η .
         (Enum α, Unsigned ν, Integral ν,
          AsBoundedError ε I64, MonadError ε η)⇒
         ν → η α
toEnum' = GHC.Enum.toEnum ∘ i64ToInt ⩺ ı

--------------------

toEnum_ ∷ ∀ ν α . (Unsigned ν, Integral ν, Enum α) ⇒ ν → α
toEnum_ = GHC.Enum.toEnum ∘ i64ToInt ∘ ɨ

----------------------------------------

allEnum ∷ Enum α ⇒ [α]
allEnum = GHC.Enum.enumFrom (toEnum_ @ℕ 0)

-- that's all, folks! ----------------------------------------------------------
