{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE UnicodeSyntax              #-}

module Natural
  ( AtMost(Cons, Nil)
  , Countable(count)
  , Four
  , I64
  , Length(len, len_, length)
  , Nat(S, Z)
  , Natty(Sy, Zy)
  , None
  , NumSign(..)
  , One
  , Replicate(replicate, replicate_)
  , Three
  , Two
  , Unsigned(boundMax, boundMax', fromI, fromI0, fromI', fromI_, ı, ị, ɨ)
  , ℕ
  , abs
  , allEnum
  , atMost
  , atMostOne
  , atMostTwo
  , four
  , fromEnum
  , fromEnum_
  , natNeg
  , none
  , one
  , propOpRespectsBounds
  , three
  , toEnum
  , toEnum'
  , toEnum_
  , two
  , unNegate
  , zeroOneOrTwo
  , (⊕)
  , (⊖)
  , (⨹)
  , (⨺)
  , (⨻)
  ) where

import Debug.Trace ( traceShow )

import Base0T hiding ( abs, (÷), (⊕) )
import Base0T qualified

import GHC.Enum qualified
import GHC.Num qualified
import GHC.Real qualified

import GHC.Enum  ( Bounded, maxBound )
import GHC.Float ( Double, logBase )
import GHC.Num   ( Num )
import GHC.Real  ( Integral, Real, divMod, fromIntegral, toRational )

-- base --------------------------------

import Data.Foldable qualified
import Data.List qualified

import Control.Applicative ( Alternative )
import Data.Bits           ( FiniteBits(finiteBitSize), countLeadingZeros,
                             oneBits, testBit, xor, (.&.), (.<<.), (.>>.) )
import Data.Bool           ( Bool(True) )
import Data.Either         ( isLeft )
import Data.Foldable       ( Foldable, elem )
import Data.Function       ( flip )
import Data.Int            ( Int16, Int32, Int64, Int8 )
import Data.Kind           ( Type )
import Data.List           ( dropWhile, zip )
import Data.Ord            ( Ordering(EQ, GT, LT), compare )
import Data.Ratio          ( Ratio, denominator, numerator, (%) )
import Data.Tuple          ( uncurry )
import Data.Typeable       ( typeOf )
import GHC.Exts            ( Int )
import Prelude             ( Enum, error )

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
import Data.MoreUnicode.Bool        ( 𝔹, pattern 𝕱, pattern 𝕿 )
import Data.MoreUnicode.Either      ( 𝔼, pattern 𝕷, pattern 𝕽 )
import Data.MoreUnicode.Functor     ( (⊳), (⩺) )
import Data.MoreUnicode.Maybe       ( 𝕄, pattern 𝕵, pattern 𝕹 )
import Data.MoreUnicode.Monoid      ( ю )
import Data.MoreUnicode.Semigroup   ( (◇) )
import Data.MoreUnicode.Text        ( 𝕋 )

-- tasty-quickcheck --------------------

import Test.Tasty.QuickCheck ( Property, property, (===) )

-- text --------------------------------

import Data.Text      qualified as Text
import Data.Text.Lazy qualified as LazyText

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import Natural.BoundedError ( AsBoundedError, BoundedError, bound,
                              throwLowerBoundError, throwUpperBoundError )

--------------------------------------------------------------------------------

data NumSign = SignPlus | SignMinus deriving (Eq, Show)

------------------------------------------------------------

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
  Zy     == Zy     = True
{-
    • Couldn't match type ‘'Z’ with ‘'S n1’
      Inaccessible code in
        a pattern with constructor:
          Sy :: forall (n :: Nat). Natty n -> Natty ('S n),
        in an equation for ‘==’
    • In the pattern: Sy _
      In an equation for ‘==’: Zy == (Sy _) = False
      In the instance declaration for ‘Eq (Natty n)’

  Zy     == (Sy _) = False
-}
{-
    • Couldn't match type ‘'S n1’ with ‘'Z’
      Inaccessible code in
        a pattern with constructor: Zy :: Natty 'Z, in an equation for ‘==’
    • In the pattern: Zy
      In an equation for ‘==’: (Sy _) == Zy = False
      In the instance declaration for ‘Eq (Natty n)’

  (Sy _) == Zy     = False
-}
  (Sy n) == (Sy m) = n == m

instance Ord (Natty n) where
  Zy     <= _      = True
{-
    • Couldn't match type ‘'S n1’ with ‘'Z’
      Inaccessible code in
        a pattern with constructor: Zy :: Natty 'Z, in an equation for ‘<=’
    • In the pattern: Zy
      In an equation for ‘<=’: (Sy _) <= Zy = False
      In the instance declaration for ‘Ord (Natty n)’

  (Sy _) <= Zy     = False
-}
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

class Typeable ν ⇒ Unsigned ν where
  {-| like `maxBound`, but 𝕹 if no upper bound -}
  boundMax' ∷ Integral ν ⇒ ν → 𝕄 ν
  {-| like `maxBound`, but as an ℤ, or 𝕹 if no upper bound -}
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
                       𝕹 → return i'
                       𝕵 m → if toInteger i > toInteger m
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
                               𝕹   → i'
                               𝕵 m → if toInteger i > m
                                     then fromIntegral m
                                     else i'

  {-| convert from a general integral type, error as necessary -}
  fromI_  ∷ (Integral ν, Integral β, Show ν) ⇒ β → ν
  fromI_ = eBound ∘ fromI
  ɨ ∷ (Integral ν, Show ν, Integral β) ⇒ β → ν
  ɨ = fromI_

--------------------

instance Unsigned ℕ where
  boundMax' _ = 𝕹

--------------------

instance Unsigned Word8 where
  boundMax' _ = 𝕵 $ maxBound @Word8

instance Unsigned Word16 where
  boundMax' _ = 𝕵 $ maxBound @Word16

instance Unsigned Word32 where
  boundMax' _ = 𝕵 $ maxBound @Word32

instance Unsigned Word64 where
  boundMax' _ = 𝕵 $ maxBound @Word64

------------------------------------------------------------

{-| Like Int64, but unsigned.  Notably, maxBound @I64 ≡ maxBound @Int64;
    which is maxBound @64 ÷ 2 -}
newtype I64 = I64 Word64
  deriving newtype (Enum, Eq, Integral, Num, Ord, Real, Show)

instance Unsigned I64 where
  boundMax' _ = 𝕵 $ fromIntegral (maxBound @Int64)

instance Bounded I64 where
  minBound = 0
  maxBound = fromIntegral $ maxBound @Int64

i64ToInt ∷ I64 → Int
i64ToInt (I64 w) = fromIntegral w

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

class Replicate α ν | α -> ν where
  {-| build an instance of type `α` from an unsigned number of `Item α`.
      In practice, for all currently-supported types
      ([β], (Lazy)Text, (Lazy)ByteString); (any type whose maxBound is ≥
      maxBound @Int64): there will be no error.
   -}
  replicate ∷ ∀ ε η.(Unsigned ν,Integral ν,AsBoundedError ε ν,MonadError ε η)⇒
              ν → Item α → η α

  -- in practice, there will be no error with anything that fits into an I64,
  -- that is, [0,maxBound @Int64]
  {-| as `replicate`, but any error is thrown as a runtime `BoundedError` -}
  replicate_ ∷ (Unsigned ν, Integral ν, Show ν) ⇒ ν → Item α → α
  replicate_ n = eBound ∘ replicate n

instance Replicate [α] I64 where
  replicate n c = flip Data.List.replicate c ∘ fromIntegral ⊳ ı n

instance Replicate 𝕋 I64 where
  replicate n c = flip Text.replicate (Text.singleton c) ∘ fromIntegral ⊳ ı n

instance Replicate LazyText.Text I64 where
  replicate n c =
    flip LazyText.replicate (LazyText.singleton c) ∘ fromIntegral ⊳ ı n

instance Replicate BS.ByteString I64 where
  replicate n c = flip BS.replicate c ∘ fromIntegral ⊳ ı n

instance Replicate BSL.ByteString I64 where
  replicate n c =
    flip BSL.replicate c ∘ fromIntegral ⊳ ı n

----------------------------------------

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
  if 𝕿
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

----------------------------------------

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

   We use circled operators (⊕), (⊖), (⊗) for error-generating arithmetic.

   The triangle operators (⨹), (⨺), (⨻) are as for the circled operators; but
   specialized to BoundedError.

   We use squared operators (⊞), (⊟), (⊠) for bounded arithmetic: things that
   would fall off the end of the line (negative, or greater than maxBound) just
   stick at the limit (that is, 0 or maxBound).  E.g., `maxBound ⊞ 2` ≡ maxBound
-}


{- | `maxBound`, for some input type -}
mb ∷ Bounded ν ⇒ ν → ν
mb _ = maxBound

{- | Use `fromIntegral` to convert an ℤ to an instance of the type of some other
     `Num` -}
asb ∷ Num α ⇒ α → ℤ → α
asb _ z = fromIntegral z

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
    ((𝕿,𝕿) : _ ) → throwUpperBoundError (typeOf a) (ĩ a + ĩ b) (mb a)
    _            → return $ a + b


(⨹) ∷ ∀ ν η . (Unsigned ν, Integral ν, Bounded ν, FiniteBits ν,
               MonadError (BoundedError ν) η) ⇒ ν → ν → η ν
(⨹) = (⊕)

(⊞) ∷ ∀ ν η . (Unsigned ν, Integral ν, Bounded ν, FiniteBits ν) ⇒ ν → ν → ν
a ⊞ b = case a ⨹ b of
          𝕽 c → c
          𝕷 e → bound e

(⊖) ∷ ∀ ε ν η . (Unsigned ν, Integral ν, AsBoundedError ε ν, MonadError ε η) ⇒
       ν → ν → η ν
a ⊖ b = if b > a
        then throwLowerBoundError (typeOf a) (ĩ a - ĩ b)  0
        else return $ a - b

(⨺) ∷ ∀ ν η . (Unsigned ν,Integral ν,MonadError (BoundedError ν) η)⇒ν → ν → η ν
(⨺) = (⊖)

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
                        (𝕿,_) → tooBig
                        (_,𝕿) → tooBig
                        (𝕱,𝕱) → foldM (⊕) 0 [ hi a × lo b .<<. w₂
                                            , hi b × lo a .<<. w₂
                                            , lo a × lo b
                                            ]

(⨻) ∷ ∀ ν η . (Unsigned ν, Integral ν, Bounded ν, FiniteBits ν,
               MonadError (BoundedError ν) η) ⇒ ν → ν → η ν
(⨻) = (⊗)

{-| Perform a bounded operation; compare the result to a given ℤ equivalent;
    if the equivalent function would produce an out-of-bounds result, then
    our bounded operation should give a BoundedError; else, it should produce
    a bounded equivalent to the Integer value. -}
{-
propOpRespectsBounds ∷ (Unsigned α,Integral α,Bounded α,FiniteBits α,Show α) ⇒
                       (∀ β . (Unsigned β,Integral β,Bounded β,FiniteBits β) ⇒
                         β → β → 𝔼 (BoundedError α) β)
                     → (ℤ → ℤ → ℤ) → α → α → Property
-}
propOpRespectsBounds ∷ (Unsigned β,Integral β,Bounded β,FiniteBits β,Show β) ⇒
                       ( β → β → 𝔼 (BoundedError β) β)
                     → (ℤ → ℤ → ℤ) → β → β → Property
propOpRespectsBounds f g a b =
  let x = g (toInteger a) (toInteger b)
  in  if x ≡ toInteger (asb a x)
      then (toInteger ⊳ f a b) === 𝕽 x
      else property $ isLeft (f a b)

{-| split an integer into a natural number and a `NumSign` -}
unNegate ∷ ℤ → (NumSign,ℕ)
unNegate n | n < 0     = (SignMinus, GHC.Real.fromIntegral $ abs n)
           | otherwise = (SignPlus,  GHC.Real.fromIntegral n)

----------------------------------------

type RatioN = Ratio ℕ

class (Ord α, Num α) ⇒ Abs α where
  type Abs' α ∷ Type
  abs ∷ α → Abs' α
  abs' ∷ α → α
  abs'' ∷ α → (NumSign,Abs' α)
  abs'' n | n < 0     = (SignMinus, abs n)
         | otherwise = (SignPlus, abs n)

instance Abs ℤ where
  type Abs' ℤ = ℕ
  abs = fromIntegral ∘ Base0T.abs
  abs' = Base0T.abs

instance Abs Int64 where
  type Abs' Int64 = Word64
  abs = fromIntegral ∘ Base0T.abs
  abs' = Base0T.abs

instance Abs Int32 where
  type Abs' Int32 = Word32
  abs = fromIntegral ∘ Base0T.abs
  abs' = Base0T.abs

instance Abs Int16 where
  type Abs' Int16 = Word16
  abs = fromIntegral ∘ Base0T.abs
  abs' = Base0T.abs

instance Abs Int8 where
  type Abs' Int8 = Word8
  abs = fromIntegral ∘ Base0T.abs
  abs' = Base0T.abs

instance (Integral α, Abs α, Integral (Abs' α)) ⇒ Abs (Ratio α) where
  type Abs' (Ratio α) = Ratio (Abs' α)
  abs a = abs (numerator a) ÷ abs (denominator a)

toRatioN ∷ Real α ⇒ α → (NumSign, RatioN)
toRatioN (toRational → a) = abs'' a

{-
class Unsigned β ⇒ Length α β | α → β where
  length    ∷ α → β
  drop      ∷ β → α → α
  take      ∷ β → α → α

instance Length 𝕋 Word64 where
  length    = fromIntegral ∘ Text.length
  drop      = Text.drop ∘ fromIntegral
  take      = Text.take ∘ fromIntegral

instance Length LT.Text Word64 where
  length = fromIntegral ∘ LT.length
  drop   = LT.drop ∘ fromIntegral
  take   = LT.take ∘ fromIntegral

-}
{-
instance Length ByteString where
  length = fromIntegral ∘ BS.length

instance Length LBS.ByteString where
  length = fromIntegral ∘ LBS.length

instance Length BS8.ByteString where
  length = fromIntegral ∘ BS8.length

instance Length LBS8.ByteString where
  length = fromIntegral ∘ LBS8.length
-}

{-
class While α where
  dropWhile ∷ (Item α → 𝔹) → α → α
  takeWhile ∷ (Item α → 𝔹) → α → α

instance While [α] where
  dropWhile = Data.List.dropWhile
  takeWhile = Data.List.takeWhile

instance While 𝕋 where
  dropWhile = Text.dropWhile
  takeWhile = Text.takeWhile

instance While LT.Text where
  dropWhile = LT.dropWhile
  takeWhile = LT.takeWhile
-}

-- … and ByteStrings …

ӿ ∷ Printable ε ⇒ 𝔼 ε α → α
ӿ = \ case 𝕷 e → error (toString e); 𝕽 r → r

ⵥ ∷ Printable ε ⇒ 𝔼 ε α → α
ⵥ = \ case 𝕷 e → error (toString e); 𝕽 r → r

infix 4 ≷
-- (≷) ∷ Ord α ⇒ α → α → Ordering
(≷) = compare

class Member α where
  type MemberItem α ∷ Type

  (∈) ∷ Eq (MemberItem α) ⇒ MemberItem α → α → 𝔹

instance Foldable ψ ⇒ Member (ψ β) where
  type MemberItem (ψ β) = β
  (∈) = elem

(÷) ∷ Integral α ⇒ α → α → Ratio α
(÷) = (%)

-- that's all, folks! ----------------------------------------------------------
