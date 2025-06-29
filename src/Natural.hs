{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE UnicodeSyntax              #-}

{-| Operations and support for natural numbers, including versions of `length`,
   `replicate`, etc.; also related support for unsigned values in general; and
   some type-level natural numbers -}

module Natural
  ( AtMost(Cons, Nil)
  , Countable(count)
  , Four
  , I64
  , Length(len, len_, length, ℓ, щ, ỻ)
  , Nat(S, Z)
  , Natty(Sy, Zy)
  , None
  , One
  , Three
  , Two
  , ℕ
  , atMost
  , atMostOne
  , atMostTwo
  , four
  , natNeg
  , none
  , one
  , three
  , two
  , zeroOneOrTwo
  , (⊕)
  , (⊖)
  , (⊞)
  , (⊟)
  , (⊠)
  , (⨹)
  , (⨺)
  , (⨻)
  ) where

import Base0T hiding ( abs, (÷), (⊕) )

import GHC.Enum ( Bounded, maxBound )
import GHC.Real ( Integral, divMod )

-- base --------------------------------

import Data.Foldable qualified

import Control.Applicative ( Alternative )
import Data.Bits           ( FiniteBits(finiteBitSize), countLeadingZeros,
                             oneBits, testBit, xor, (.&.), (.<<.), (.>>.) )
import Data.Foldable       ( Foldable )
import Data.List           ( zip )
import Data.Ord            ( Ordering(EQ, GT, LT) )
import Data.Tuple          ( uncurry )
import Data.Typeable       ( typeOf )
import Prelude             ( error )

-- base-unicode-symbols ----------------

import Prelude.Unicode ( (×) )

-- bytestring --------------------------

import Data.ByteString      qualified as BS
import Data.ByteString.Lazy qualified as BSL

-- lens --------------------------------

import Control.Lens.Setter    ( over )
import Control.Lens.Traversal ( both )

-- more-unicode ------------------------

import Data.MoreUnicode.Applicative ( (∤), (⊵) )
import Data.MoreUnicode.Bool        ( pattern 𝓕, pattern 𝓣 )
import Data.MoreUnicode.Either      ( 𝔼, pattern 𝓛, pattern 𝓡 )
import Data.MoreUnicode.Functor     ( (⊳) )
import Data.MoreUnicode.Maybe       ( ⅎ )
import Data.MoreUnicode.Monoid      ( ю )
import Data.MoreUnicode.Ord         ( (≷) )
import Data.MoreUnicode.Semigroup   ( (◇) )
import Data.MoreUnicode.Text        ( 𝕋 )

-- text --------------------------------

import Data.Text      qualified as Text
import Data.Text.Lazy qualified as LazyText

-- while -------------------------------

import While ( dropWhile )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import Natural.BoundedError ( AsBoundedError,
                              BEType(LowerBoundType, UpperBoundType),
                              BoundedError, bound, boundedErrorType,
                              throwLowerBoundError, throwUpperBoundError )
import Natural.Unsigned     ( I64, Unsigned(boundMax', ı) )

--------------------------------------------------------------------------------

class Countable α where
  count :: α → ℕ

instance Countable (AtMost n a) where
  count Nil        = 0
  count (Cons _ x) = 1 + count x

------------------------------------------------------------

{- Thank you to dfeuer, redneb, leftroundabout on StackOverflow
   http://stackoverflow.com/questions/39690844/haskell-how-do-i-create-a-function-that-allows-none-one-or-two-applicatives
-}

data Nat = Z
         | S Nat
  deriving (Eq, Ord, Show)

instance Countable Nat where
  count Z     = 0
  count (S n) = 1 + count n

------------------------------------------------------------

data Natty n where Zy :: Natty 'Z
                   Sy :: Natty n -> Natty ('S n)

instance Countable (Natty n) where
  count Zy     = 0
  count (Sy n) = 1 + count n

_show ∷ Natty n → String
_show Zy     = "'Z"
_show (Sy n) = "'S " ◇ _show n

instance Show (Natty n) where
  show z = "Natty " ◇ _show z

instance Eq (Natty n) where
  Zy     == Zy     = 𝓣
  (Sy n) == (Sy m) = n == m

instance Ord (Natty n) where
  Zy     <= _      = 𝓣
  (Sy n) <= (Sy m) = n <= m

------------------------------------------------------------

data AtMost n a where Nil :: AtMost n a
                      Cons :: a -> AtMost n a -> AtMost ('S n) a

instance Eq (AtMost n a) where
  a == b = (count a) == (count b)

instance Ord (AtMost n a) where
  a <= b = (count a) <= (count b)

------------------------------------------------------------

atMost ∷ Alternative f ⇒ Natty n → f a → f (AtMost n a)
atMost Zy _     = pure Nil
atMost (Sy n) a = (Cons ⊳ a ⊵ atMost n a) ∤ pure Nil

atMostOne ∷ Alternative f ⇒ f a → f (AtMost One a)
atMostOne = atMost (Sy Zy)
atMostTwo ∷ Alternative f ⇒ f a → f (AtMost Two a)
atMostTwo = atMost (Sy (Sy Zy))

type None  = 'Z
type One   = 'S None
type Two   = 'S One -- ('S 'Z)
type Three = 'S Two
type Four  = 'S Three

none ∷ Natty 'Z
none = Zy

one ∷ Natty ('S 'Z)
one  = Sy none

two ∷ Natty ('S ('S 'Z))
two  = Sy one

three ∷ Natty ('S ('S ('S 'Z)))
three  = Sy two

four ∷ Natty ('S ('S ('S ('S 'Z))))
four  = Sy three

eBound ∷ Show ω ⇒ 𝔼 (BoundedError ω) ν → ν
eBound = either (error ∘ show) id

------------------------------------------------------------

class (Unsigned ν, Integral ν) ⇒ Length α ν | α → ν where
  {-| get the unsigned length of a thing; will error if the type cannot
      represent the value.  In practice, for all currently-supported types
      (Foldable, (Lazy)Text, (Lazy)ByteString); (any type whose maxBound is ≥
      maxBound @Int64): there will be no error. -}
  len ∷ ∀ ε η . (AsBoundedError ε ν, MonadError ε η) ⇒ α → η ν
  {-| unicode alias for `len` -}
  ℓ ∷ ∀ ε η . (AsBoundedError ε ν, MonadError ε η) ⇒ α → η ν
  ℓ = len

  {-| as `len`, but any error is thrown as a runtime `BoundedError` -}
  len_ ∷ Show  ν ⇒ α → ν
  len_ = eBound ∘ len
  {-| unicode alias for `len_` -}
  щ ∷ Show ν ⇒ α → ν
  щ = len_

  {-| `len_`, specialized to ℕ -}
  length ∷ Show ν ⇒ α → ℕ
  length = fromIntegral ∘ len_

  {-| unicode alias for `length` -}
  ỻ ∷ Show ν ⇒ α → ℕ
  ỻ = length

--------------------

instance Foldable ψ ⇒ Length (ψ α) Word64 where
  len = ı ∘ Data.Foldable.length

--------------------

instance Length 𝕋 Word64 where
  len    = ı ∘ Text.length

--------------------

instance Length LazyText.Text Word64 where
  len    = ı ∘ LazyText.length

--------------------

instance Length BS.ByteString Word64 where
  len = ı ∘ BS.length

--------------------

instance Length BSL.ByteString Word64 where
  len = ı ∘ BSL.length

------------------------------------------------------------

zeroOneOrTwo ∷ Alternative f ⇒ f a → f [a]
zeroOneOrTwo a = go (2 :: ℕ)
  where
    go n
      | n > 0 = ((:) ⊳ a ⊵ go (n - 1)) ∤ pure []
      | otherwise = pure []

----------------------------------------

{-| subtract `y` from `x`, but if that would go negative, return 0 -}
natNeg ∷ ℕ → ℕ → ℕ
natNeg x y = if x ≥ y then x - y else 0

{- Standard operators - (+), (-), (*) perform modulo arithmetic; this is
   consistent with the standard library.  Where that makes no sense (e.g.,
   `(0∷ℕ)-1`); it errors (arithmetic underflow).

   We use circled operators (⊕), (⊖), (⊗) for error-generating arithmetic;
   that is, each operates within a MonadError (AsBoundedError) context.

   The triangle operators (⨹), (⨺), (⨻) are as for the circled operators; but
   specialized to BoundedError.

   We use squared operators (⊞), (⊟), (⊠) for bounded arithmetic: things that
   would fall off the end of the line (negative, or greater than maxBound) just
   stick at the limit (that is, 0 or maxBound).  E.g., `maxBound ⊞ 2` ≡ maxBound
-}


{- | `maxBound`, for some input type -}
mb ∷ Bounded ν ⇒ ν → ν
mb _ = maxBound

{- | the simple binary repr of an unsigned int value; as a list of bools,
     most-significant first -}
bits ∷ FiniteBits β ⇒ β → [Bool]
bits x = testBit x ⊳ ([(finiteBitSize x)-1,(finiteBitSize x)-2..0])

ĩ ∷ Integral α ⇒ α → ℤ
ĩ = fromIntegral @_ @ℤ

(⊕) ∷ ∀ ε ν η . (Unsigned ν, Integral ν, Bounded ν, FiniteBits ν,
                 AsBoundedError ε ν, MonadError ε η) ⇒ ν → ν → η ν
a ⊕ b =
  case dropWhile (uncurry xor) $ (zip (bits a) (bits b)) of
    ((𝓣,𝓣) : _ ) → throwUpperBoundError (typeOf a) (ĩ a + ĩ b) (mb a)
    _            → return $ a + b


(⨹) ∷ ∀ ν η . (Unsigned ν, Integral ν, Bounded ν, FiniteBits ν,
               MonadError (BoundedError ν) η) ⇒ ν → ν → η ν
(⨹) = (⊕)

(⊞) ∷ (Unsigned ν, Integral ν, Bounded ν, FiniteBits ν) ⇒ ν → ν → ν
a ⊞ b = case a ⨹ b of
          𝓡 c → c
          𝓛 e → bound e

(⊖) ∷ ∀ ε ν η . (Unsigned ν, Integral ν, AsBoundedError ε ν, MonadError ε η) ⇒
       ν → ν → η ν
a ⊖ b = if b > a
        then throwLowerBoundError (typeOf a) (ĩ a - ĩ b)  0
        else return $ a - b

(⨺) ∷ ∀ ν η . (Unsigned ν,Integral ν,MonadError (BoundedError ν) η)⇒ν → ν → η ν
(⨺) = (⊖)

(⊟) ∷ (Unsigned ν, Integral ν, Bounded ν, FiniteBits ν) ⇒ ν → ν → ν
a ⊟ b = case a ⨺ b of
          𝓡 c → c
          𝓛 _ → 0

(⊗) ∷ ∀ ε ν η . (Unsigned ν, Integral ν, Bounded ν, FiniteBits ν,
                 AsBoundedError ε ν, MonadError ε η) ⇒
      ν → ν → η ν
a ⊗ b = do
  let bitWidth ∷ (Integral α, FiniteBits α, Unsigned α, Integral β)⇒ α→β
      bitWidth x = fromIntegral $ finiteBitSize x - countLeadingZeros x
      w = finiteBitSize a
      w₂ = case w `divMod` 2 of
             (_,1) → error $ ю [ "odd bit widths unsupported (got ",show w,")" ]
             (y,_) → y

      loBits ∷ (Integral α, FiniteBits α, Unsigned α) ⇒ α → α
      loBits _ = oneBits .>>. (fromIntegral w₂)

      lo ∷ (Integral α, FiniteBits α, Unsigned α) ⇒ α → α
      lo x = x .&. loBits x

      hi ∷ (Integral α, FiniteBits α, Unsigned α) ⇒ α → α
      hi x = x .>>. (fromIntegral w₂)

      tooBig = throwUpperBoundError (typeOf a) (ĩ a × ĩ b) (mb a)
  case bitWidth a + bitWidth b ≷ (1 + finiteBitSize a) of
                 GT → tooBig
                 LT → return $ a × b
                 EQ → case over both (> loBits a) (hi a × lo b, hi b × lo a) of
                        (𝓣,_) → tooBig
                        (_,𝓣) → tooBig
                        (𝓕,𝓕) → foldM (⊕) 0 [ hi a × lo b .<<. w₂
                                            , hi b × lo a .<<. w₂
                                            , lo a × lo b
                                            ]

(⨻) ∷ ∀ ν η . (Unsigned ν, Integral ν, Bounded ν, FiniteBits ν,
               MonadError (BoundedError ν) η) ⇒ ν → ν → η ν
(⨻) = (⊗)

(⊠) ∷ (Unsigned β, Integral β, Bounded β, FiniteBits β) ⇒ β → β → β
a ⊠ b = case a ⊗ b of
          𝓛 e -> case boundedErrorType e of
                   UpperBoundType -> ⅎ (boundMax' a)
                   LowerBoundType -> 0
          𝓡 r -> r

-- that's all, folks! ----------------------------------------------------------
