{-# LANGUAGE UnicodeSyntax #-}
module Natural.Length
  ( Length(len, ℓ, len_, щ, length, ỻ)
  ) where

import Base0T

-- base --------------------------------

import Data.Foldable qualified

import Data.Foldable ( Foldable )
import Prelude       ( Integral, error )

-- bytestring --------------------------

import Data.ByteString      qualified as BS
import Data.ByteString.Lazy qualified as BSL

-- more-unicode ------------------------

import Data.MoreUnicode.Either ( 𝔼 )
import Data.MoreUnicode.Text   ( 𝕋 )

-- text --------------------------------

import Data.Text      qualified as Text
import Data.Text.Lazy qualified as LazyText

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import Natural.BoundedError ( AsBoundedError, BoundedError )
import Natural.Unsigned     ( I64, Unsigned(ı) )

--------------------------------------------------------------------------------

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

instance Foldable ψ ⇒ Length (ψ α) I64 where
  len = ı ∘ Data.Foldable.length

--------------------

instance Length 𝕋 I64 where
  len    = ı ∘ Text.length

--------------------

instance Length LazyText.Text I64 where
  len    = ı ∘ LazyText.length

--------------------

instance Length BS.ByteString I64 where
  len = ı ∘ BS.length

--------------------

instance Length BSL.ByteString I64 where
  len = ı ∘ BSL.length

-- that's all, folks! ----------------------------------------------------------
