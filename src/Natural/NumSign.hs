{-# LANGUAGE UnicodeSyntax #-}
module Natural.NumSign
  ( NumSign(SignPlus, SignMinus)
  ) where

{-| identifier for (e.g.,) abs, to say whether a value is greater or less than zero -}

-- base --------------------------------

import Data.Eq   ( Eq )
import Text.Show ( Show )

--------------------------------------------------------------------------------

{-| identifier for (e.g.,) abs, to say whether a value is greater or less than zero -}
data NumSign = SignPlus | SignMinus deriving (Eq, Show)

-- that's all, folks! ----------------------------------------------------------
