{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{-# LANGUAGE UnicodeSyntax              #-}
module Natural.Unsigned
  ( I16(I16)
  , I32(I32)
  , I64(I64)
  , I8(I8)
  , Unsigned(boundMax, boundMax', fromI, fromI0, fromI', fromI_, ı, ị, ɨ)
  , i16ToInt
  , i32ToInt
  , i64ToInt
  , i8ToInt
  ) where

import Base0T

-- base --------------------------------

import Data.Bits     ( Bits,
                       FiniteBits(countLeadingZeros, countTrailingZeros, finiteBitSize) )
import Data.Int      ( Int, Int16, Int32, Int64, Int8 )
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
  deriving newtype (Bits, Enum, Eq, Integral, Num, Ord, Real, Show)

instance Unsigned I64 where
  boundMax' _ = 𝓙 $ fromIntegral (maxBound @Int64)

instance Bounded I64 where
  minBound = 0
  maxBound = fromIntegral $ maxBound @Int64

instance FiniteBits I64 where
  finiteBitSize = const 63
  countLeadingZeros (I64 n) = countLeadingZeros n
  countTrailingZeros (I64 n) = countTrailingZeros n

i64ToInt ∷ I64 → Int
i64ToInt (I64 w) = fromIntegral w

------------------------------------------------------------

{-| Like Int32, but unsigned.  Notably, maxBound @I32 ≡ maxBound @Int32;
    which is maxBound @Word32 ÷ 2 -}
newtype I32 = I32 Word32
  deriving newtype (Bits, Enum, Eq, Integral, Num, Ord, Real, Show)

instance Unsigned I32 where
  boundMax' _ = 𝓙 $ fromIntegral (maxBound @Int32)

instance Bounded I32 where
  minBound = 0
  maxBound = fromIntegral $ maxBound @Int32

instance FiniteBits I32 where
  finiteBitSize = const 31
  countLeadingZeros (I32 n) = countLeadingZeros n
  countTrailingZeros (I32 n) = countTrailingZeros n

i32ToInt ∷ I32 → Int
i32ToInt (I32 w) = fromIntegral w

------------------------------------------------------------

{-| Like Int16, but unsigned.  Notably, maxBound @I16 ≡ maxBound @Int16;
    which is maxBound @Word16 ÷ 2 -}
newtype I16 = I16 Word16
  deriving newtype (Bits, Enum, Eq, Integral, Num, Ord, Real, Show)

instance Unsigned I16 where
  boundMax' _ = 𝓙 $ fromIntegral (maxBound @Int16)

instance Bounded I16 where
  minBound = 0
  maxBound = fromIntegral $ maxBound @Int16

instance FiniteBits I16 where
  finiteBitSize = const 15
  countLeadingZeros (I16 n) = countLeadingZeros n
  countTrailingZeros (I16 n) = countTrailingZeros n

i16ToInt ∷ I16 → Int
i16ToInt (I16 w) = fromIntegral w

------------------------------------------------------------

{-| Like Int8, but unsigned.  Notably, maxBound @I8 ≡ maxBound @Int8;
    which is maxBound @Word8 ÷ 2 -}
newtype I8 = I8 Word8
  deriving newtype (Bits, Enum, Eq, Integral, Num, Ord, Real, Show)

instance Unsigned I8 where
  boundMax' _ = 𝓙 $ fromIntegral (maxBound @Int8)

instance Bounded I8 where
  minBound = 0
  maxBound = fromIntegral $ maxBound @Int8

instance FiniteBits I8 where
  finiteBitSize = const 7
  countLeadingZeros (I8 n) = countLeadingZeros n
  countTrailingZeros (I8 n) = countTrailingZeros n

i8ToInt ∷ I8 → Int
i8ToInt (I8 w) = fromIntegral w

-- that's all, folks! ----------------------------------------------------------
