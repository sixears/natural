{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE UnicodeSyntax     #-}

module Natural
  ( AtMost( Cons, Nil ), Countable( count ), Nat( S, Z ), Natty( Sy, Zy), None
  , ℕ
  , atMost, atMostOne, atMostTwo
  , zeroOneOrTwo

  , One, Two, Three, Four
  , none, one, two, three, four
  , allEnum, fromEnum, length, replicate, toEnum
  )
where

import qualified  GHC.Enum
import qualified  GHC.Num
import qualified  GHC.Real
import GHC.Num  ( (+), (-) )

-- base --------------------------------

import qualified  Data.Foldable
import qualified  Data.List

import Control.Applicative  ( Alternative, pure )
import Data.Bool            ( Bool( True ), otherwise )
import Data.Eq              ( Eq( (==) ) )
import Data.Foldable        ( Foldable )
import Data.Ord             ( Ord( (<=), (>) ) )
import Data.String          ( String )
import GHC.Exts             ( IsList( Item ) )
import Text.Show            ( Show( show ) )

-- base-unicode-symbols ----------------

import Data.Function.Unicode  ( (∘) )
import Data.Monoid.Unicode    ( (⊕) )

-- more-unicode ------------------------

import Data.MoreUnicode.Applicative  ( (∤), (⊵) )
import Data.MoreUnicode.Functor      ( (⊳) )
import Data.MoreUnicode.Natural      ( ℕ )
import Data.MoreUnicode.Text         ( 𝕋 )

-- text --------------------------------

import qualified  Data.Text

--------------------------------------------------------------------------------

class Countable α where
  count :: α -> ℕ

instance Countable (AtMost n a) where
  count Nil        = 0
  count (Cons _ x) = 1 + count x

------------------------------------------------------------

{- Thank you to dfeuer, redneb, leftroundabout on StackOverflow
   http://stackoverflow.com/questions/39690844/haskell-how-do-i-create-a-function-that-allows-none-one-or-two-applicatives
-}

data Nat = Z | S Nat
  deriving (Eq, Ord, Show)

instance Countable Nat where
  count Z = 0
  count (S n) = 1 + count n

------------------------------------------------------------

data Natty n where
  Zy :: Natty 'Z
  Sy :: Natty n -> Natty ('S n)

instance Countable (Natty n) where
  count Zy = 0
  count (Sy n) = 1 + count n

_show ∷ Natty n → String
_show Zy = "'Z"
_show (Sy n) = "'S " ⊕ _show n

instance Show (Natty n) where
  show z = "Natty " ⊕ _show z

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

data AtMost n a where
  Nil :: AtMost n a
  Cons :: a -> AtMost n a -> AtMost ('S n) a

instance Eq (AtMost n a) where
  a == b = (count a) == (count b)

instance Ord (AtMost n a) where
  a <= b = (count a) <= (count b)

------------------------------------------------------------

atMost :: Alternative f => Natty n -> f a -> f (AtMost n a)
atMost Zy _ = pure Nil
atMost (Sy n) a = (Cons ⊳ a ⊵ atMost n a) ∤ pure Nil

atMostOne :: Alternative f => f a -> f (AtMost One a)
atMostOne = atMost (Sy Zy)
atMostTwo :: Alternative f => f a -> f (AtMost Two a)
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

------------------------------------------------------------

class Length α where
  length ∷ α → ℕ

instance Length 𝕋 where
  length = GHC.Real.fromIntegral ∘ Data.Text.length

instance Foldable ψ ⇒ Length (ψ α) where
  length = GHC.Real.fromIntegral ∘ Data.Foldable.length

class Replicate α where
  replicate ∷ ℕ -> Item α → α

instance Replicate [α] where
  replicate = Data.List.replicate ∘ GHC.Real.fromIntegral

instance Replicate 𝕋 where
  replicate n c = Data.Text.replicate (GHC.Real.fromIntegral n)
                                      (Data.Text.singleton c)

fromEnum ∷ GHC.Enum.Enum α ⇒ α → ℕ
fromEnum = GHC.Real.fromIntegral ∘ GHC.Enum.fromEnum

toEnum ∷ GHC.Enum.Enum α ⇒ ℕ → α
toEnum = GHC.Enum.toEnum ∘ GHC.Num.fromInteger ∘ GHC.Real.toInteger

allEnum ∷ GHC.Enum.Enum α ⇒ [α]
allEnum = GHC.Enum.enumFrom (toEnum 0)

zeroOneOrTwo :: Alternative f => f a -> f [a]
zeroOneOrTwo a = go (2 :: ℕ)
  where
    go n
      | n > 0 = ((:) ⊳ a ⊵ go (n - 1)) ∤ pure []
      | otherwise = pure []

-- that's all, folks! ----------------------------------------------------------
