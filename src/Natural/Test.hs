{-# LANGUAGE
    DataKinds,
    FlexibleInstances,
    MultiParamTypeClasses,
    ScopedTypeVariables,
    TypeFamilies,
    TypeOperators,
    UndecidableInstances #-}

-- https://stackoverflow.com/questions/39599346/avoiding-overlapping-instances-with-closed-type-families-ghc-advancedoverlap

-- based on example 3, closed type families

import Prelude -- hiding (print)

import Data.Bits     ( FiniteBits(finiteBitSize) )
import Data.Proxy
import Data.Word  ( Word8 )
import Data.MoreUnicode.Maybe  ( 𝕄, pattern 𝓙, pattern 𝓝 )
import Data.Function.Unicode  ( (∘) )
import Numeric.Natural.Unicode  ( ℕ )

data HTrue
data HFalse

type family HasFiniteBits a where
  HasFiniteBits Word8 = HTrue
  HasFiniteBits a     = HFalse

class MaybeFiniteBitSize' θ α where
  maybeFiniteBitSize' :: Proxy θ -> α -> 𝕄 Int

instance (FiniteBits α, Show α) => MaybeFiniteBitSize' HTrue α where
  maybeFiniteBitSize'     _ = 𝓙 ∘ finiteBitSize

instance MaybeFiniteBitSize' HFalse α where
  maybeFiniteBitSize' _ = const 𝓝

class MaybeFiniteBitSize α where
  maybeFiniteBitSize :: α -> 𝕄 Int

-- the HasFiniteBits constraint serves to unify θ with HTrue or HFalse, as per the type
-- of α, which thus selects the implementation of MaybeFiniteBitSize' to use
instance (HasFiniteBits α ~ θ, MaybeFiniteBitSize' θ α) => MaybeFiniteBitSize α where
  maybeFiniteBitSize = maybeFiniteBitSize' (Proxy :: Proxy θ)

main :: IO ()
main = do
  print (maybeFiniteBitSize (7 :: Word8))
  print (maybeFiniteBitSize (7 :: ℕ))
