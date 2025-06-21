{-# LANGUAGE UnicodeSyntax #-}
module Natural.Countable
  ( Countable(count, ͻ)
  ) where

import Base0T

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import Natural.Unsigned ( I64 )

--------------------------------------------------------------------------------

class Countable α where
  count :: α → ℕ
  ͻ :: α → ℕ
  ͻ = count

instance Countable ℕ where
  count = id

instance Countable Word8 where
  count = fromIntegral

instance Countable Word16 where
  count = fromIntegral

instance Countable Word32 where
  count = fromIntegral

instance Countable Word64 where
  count = fromIntegral

instance Countable I64 where
  count = fromIntegral

-- that's all, folks! ----------------------------------------------------------
