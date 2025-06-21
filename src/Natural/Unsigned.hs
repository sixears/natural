{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{-# LANGUAGE UnicodeSyntax              #-}
module Natural.Unsigned
  ( I64(I64)
  , Unsigned(boundMax, boundMax', fromI, fromI0, fromI', fromI_, ı, ị, ɨ)
  , i64ToInt
  ) where

import Base0T

-- base --------------------------------

import Data.Int      ( Int, Int64 )
import Data.Typeable ( typeOf )
import Prelude       ( Bounded(maxBound, minBound), Enum, Integral, Num, Real )

-- more-unicode ------------------------

import Data.MoreUnicode.Either  ( 𝔼, ⵥ )
import Data.MoreUnicode.Functor ( (⩺) )
import Data.MoreUnicode.Maybe   ( 𝕄, pattern 𝓙, pattern 𝓝 )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import Natural.BoundedError ( AsBoundedError, BoundedError,
                              throwLowerBoundError, throwUpperBoundError )

--------------------------------------------------------------------------------

class Typeable ν ⇒ Unsigned ν where
  {-| like `maxBound`, but 𝓝 if no upper bound -}
  boundMax' ∷ Integral ν ⇒ ν → 𝕄 ν
  {-| like `maxBound`, but as an ℤ, or 𝓝 if no upper bound -}
  boundMax ∷ Integral ν ⇒ ν → 𝕄 ℤ
  boundMax = toInteger ⩺ boundMax'

  {-| convert from a general integral type, throwing as necessary -}
  fromI ∷ ∀ ε β η . (Integral ν, Integral β, AsBoundedError ε ν,MonadError ε η)⇒
          β → η ν
  -- we cannot do this as a set of guards (e.g., `fromI i | i < 0 = ...`),
  -- because we need to pre-construct (lexically) the value i' to take its
  -- `typeOf`.  I tried implying the type from the return, etc., and using a
  -- proxy, but GHC can't handle the type inference there
  fromI i = let i' = fromIntegral i
            in  if i < 0
                then throwLowerBoundError (typeOf i') (toInteger i) 0
                else case boundMax' i' of
                       𝓝 → return i'
                       𝓙 m → if toInteger i > toInteger m
                             then throwUpperBoundError (typeOf i')
                                                       (toInteger i) m
                             else return i'

  ı ∷ ∀ ε β η . (Integral ν, Integral β, AsBoundedError ε ν, MonadError ε η) ⇒
      β → η ν
  ı = fromI

  {-| convert from a general integral type, return  0 for negative values -}
  fromI0  ∷ ∀ ε β η . (Integral ν,Integral β,AsBoundedError ε ν,MonadError ε η)⇒
            β → η ν
  fromI0 i | i < 0     = return 0
           | otherwise = fromI i
  ị ∷ ∀ ε β η . (Integral ν, Integral β, AsBoundedError ε ν, MonadError ε η) ⇒
      β → η ν
  ị= fromI0

  {-| convert from a general integral type, return 0 / maxBound for out-of-bound
      values -}
  fromI'  ∷ (Integral ν, Integral β) ⇒ β → ν
  fromI' i | i < 0     = 0
           | otherwise = let i' = fromIntegral i
                         in  case boundMax i' of
                               𝓝   → i'
                               𝓙 m → if toInteger i > m
                                     then fromIntegral m
                                     else i'

  {-| convert from a general integral type, error as necessary -}
  fromI_ ∷ (Integral ν, Integral β, Show ν) ⇒ β → ν
  fromI_ = let eBound ∷ Show ω ⇒ 𝔼 (BoundedError ω) ν → ν
               eBound = ⵥ
           in  eBound ∘ fromI
  ɨ ∷ (Integral ν, Show ν, Integral β) ⇒ β → ν
  ɨ = fromI_

--------------------

instance Unsigned ℕ where
  boundMax' _ = 𝓝

--------------------

instance Unsigned Word8 where
  boundMax' _ = 𝓙 $ maxBound @Word8

instance Unsigned Word16 where
  boundMax' _ = 𝓙 $ maxBound @Word16

instance Unsigned Word32 where
  boundMax' _ = 𝓙 $ maxBound @Word32

instance Unsigned Word64 where
  boundMax' _ = 𝓙 $ maxBound @Word64

------------------------------------------------------------

{-| Like Int64, but unsigned.  Notably, maxBound @I64 ≡ maxBound @Int64;
    which is maxBound @Word64 ÷ 2 -}
newtype I64 = I64 Word64
  deriving newtype (Enum, Eq, Integral, Num, Ord, Real, Show)

instance Unsigned I64 where
  boundMax' _ = 𝓙 $ fromIntegral (maxBound @Int64)

instance Bounded I64 where
  minBound = 0
  maxBound = fromIntegral $ maxBound @Int64

i64ToInt ∷ I64 → Int
i64ToInt (I64 w) = fromIntegral w

-- that's all, folks! ----------------------------------------------------------
