{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UnicodeSyntax        #-}
module Natural.Abs
  ( Abs(abs, abs', abs'', ꬰ, ⱥ)
  , toRatioN
  , unNegate
  ) where

import Base0T hiding ( abs, (÷) )
import Base0T qualified

-- base --------------------------------

import Prelude qualified

import Data.Int   ( Int16, Int32, Int64, Int8 )
import Data.Kind  ( Type )
import Data.Ratio ( Ratio, denominator, numerator )
import Prelude    ( Integral, Num, Real, toRational )

-- more-unicode ------------------------

import Data.MoreUnicode.Num ( (÷) )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import Natural.NumSign  ( NumSign(SignMinus, SignPlus) )
import Natural.Unsigned ( I16, I32, I64, I8 )

--------------------------------------------------------------------------------

class (Ord α, Num α) ⇒ Abs α where
  {-| when invoking `abs`, this is the target type -}
  type Abs' α ∷ Type
  {-| abs, but correcting the type -}
  abs ∷ α → Abs' α
  {-| unicode alias for `abs` -}
  ꬰ :: α → Abs' α
  ꬰ = abs
  {-| like `Prelude.abs`, maintains the type -}
  abs' ∷ α → α
  {-| like `abs'`, also return the sign of the original -}
  abs'' ∷ α → (NumSign,Abs' α)
  abs'' n | n < 0     = (SignMinus, abs n)
          | otherwise = (SignPlus, abs n)
  {-| unicode alias for `abs''` -}
  ⱥ :: α → (NumSign, Abs' α)
  ⱥ = abs''


instance Abs ℤ where
  type Abs' ℤ = ℕ
  abs = fromIntegral ∘ Base0T.abs
  abs' = Base0T.abs

instance Abs I64 where
  type Abs' I64 = I64
  abs = fromIntegral ∘ Base0T.abs
  abs' = Base0T.abs

instance Abs Int64 where
  type Abs' Int64 = I64
  abs = fromIntegral ∘ Base0T.abs
  abs' = Base0T.abs

instance Abs I32 where
  type Abs' I32 = I32
  abs = fromIntegral ∘ Base0T.abs
  abs' = Base0T.abs

instance Abs Int32 where
  type Abs' Int32 = I32
  abs = fromIntegral ∘ Base0T.abs
  abs' = Base0T.abs

instance Abs I16 where
  type Abs' I16 = I16
  abs = fromIntegral ∘ Base0T.abs
  abs' = Base0T.abs

instance Abs Int16 where
  type Abs' Int16 = I16
  abs = fromIntegral ∘ Base0T.abs
  abs' = Base0T.abs

instance Abs I8 where
  type Abs' I8 = I8
  abs = fromIntegral ∘ Base0T.abs
  abs' = Base0T.abs

instance Abs Int8 where
  type Abs' Int8 = I8
  abs = fromIntegral ∘ Base0T.abs
  abs' = Base0T.abs

instance (Integral α, Abs α, Integral (Abs' α)) ⇒ Abs (Ratio α) where
  type Abs' (Ratio α) = Ratio (Abs' α)
  abs a = abs (numerator a) ÷ abs (denominator a)
  abs' = Prelude.abs


------------------------------------------------------------

type RatioN = Ratio ℕ

{-| convert a `Real` into fraction (`RatioN`), and a `NumSign` -}
toRatioN ∷ Real α ⇒ α → (NumSign, RatioN)
toRatioN (toRational → a) = abs'' a

----------------------------------------

{-| split an integer into a natural number and a `NumSign` -}
unNegate ∷ ℤ → (NumSign,ℕ)
unNegate n | n < 0     = (SignMinus, fromIntegral $ abs n)
           | otherwise = (SignPlus,  fromIntegral n)

-- that's all, folks! ----------------------------------------------------------
