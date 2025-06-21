{-# LANGUAGE UnicodeSyntax #-}
module Natural.Replicate
  ( Replicate(replicate, replicate_, drop, drop_, take, take_)
  ) where

import Base0T

-- base --------------------------------

import Data.Function ( flip )
import Data.List qualified

import Prelude ( Integral )

-- bytestring --------------------------

import Data.ByteString      qualified as BS
import Data.ByteString.Lazy qualified as BSL

-- more-unicode ------------------------

import Data.MoreUnicode.Either  ( 𝔼, ⵥ )
import Data.MoreUnicode.Functor ( (⊳) )
import Data.MoreUnicode.Text    ( 𝕋 )

-- text --------------------------------

import Data.Text      qualified as Text
import Data.Text.Lazy qualified as LazyText

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import Natural.BoundedError ( AsBoundedError, BoundedError )
import Natural.Unsigned     ( I64, Unsigned, ı )

--------------------------------------------------------------------------------

eBound ∷ Show ω ⇒ 𝔼 (BoundedError ω) ν → ν
eBound = ⵥ

class Replicate α ν | α -> ν where
  {-| build an instance of type `α` from an unsigned number of `Item α`.
      In practice, for all currently-supported types
      ([β], (Lazy)Text, (Lazy)ByteString); (any type whose maxBound is ≥
      maxBound @Int64): there will be no error.
   -}
  replicate ∷ ∀ ε η.(Unsigned ν,Integral ν,AsBoundedError ε ν,MonadError ε η) ⇒
              ν → Item α → η α
  drop ∷ ∀ ε η . (Unsigned ν ,Integral ν, AsBoundedError ε ν ,MonadError ε η) ⇒
         ν → α → η α
  take ∷ ∀ ε η . (Unsigned ν ,Integral ν, AsBoundedError ε ν ,MonadError ε η) ⇒
         ν → α → η α

  -- in practice, there will be no error with anything that fits into an I64,
  -- that is, [0,maxBound @Int64]
  {-| as `replicate`, but any error is thrown as a runtime `BoundedError` -}
  replicate_ ∷ (Unsigned ν, Integral ν, Show ν) ⇒ ν → Item α → α
  replicate_ n = eBound ∘ replicate n
  drop_ ∷ (Unsigned ν, Integral ν, Show ν) ⇒ ν → α → α
  drop_ n = eBound ∘ drop n
  take_ ∷ (Unsigned ν, Integral ν, Show ν) ⇒ ν → α → α
  take_ n = eBound ∘ take n

instance Replicate [α] I64 where
  replicate n c = flip Data.List.replicate c ∘ fromIntegral ⊳ ı n
  drop n c = flip Data.List.drop c ∘ fromIntegral ⊳ ı n
  take n c = flip Data.List.take c ∘ fromIntegral ⊳ ı n

instance Replicate 𝕋 I64 where
  replicate n c = flip Text.replicate (Text.singleton c) ∘ fromIntegral ⊳ ı n
  drop n c = flip Text.drop c ∘ fromIntegral ⊳ ı n
  take n c = flip Text.take c ∘ fromIntegral ⊳ ı n

instance Replicate LazyText.Text I64 where
  replicate n c =
    flip LazyText.replicate (LazyText.singleton c) ∘ fromIntegral ⊳ ı n
  drop n c = flip LazyText.drop c ∘ fromIntegral ⊳ ı n
  take n c = flip LazyText.take c ∘ fromIntegral ⊳ ı n

instance Replicate BS.ByteString I64 where
  replicate n c = flip BS.replicate c ∘ fromIntegral ⊳ ı n
  drop n c = flip BS.drop c ∘ fromIntegral ⊳ ı n
  take n c = flip BS.take c ∘ fromIntegral ⊳ ı n

instance Replicate BSL.ByteString I64 where
  replicate n c =
    flip BSL.replicate c ∘ fromIntegral ⊳ ı n
  drop n c = flip BSL.drop c ∘ fromIntegral ⊳ ı n
  take n c = flip BSL.take c ∘ fromIntegral ⊳ ı n

-- that's all, folks! ----------------------------------------------------------
