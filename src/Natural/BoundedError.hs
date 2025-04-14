{-# LANGUAGE UnicodeSyntax #-}

module Natural.BoundedError
  ( AsBoundedError
  , BEType(LowerBoundType, UpperBoundType)
  , BoundedError
  , BoundedErrorType(LowerBound, UpperBound)
  , bound
  , boundedErrorType
  , lowerBoundError
  , throwLowerBoundError
  , throwUpperBoundError
  , upperBoundError
  ) where

-- base --------------------------------

import Control.Exception ( Exception )
import Data.Eq           ( Eq((==)) )
import Data.Function     ( id, ($) )
import Data.String       ( unwords )
import Data.Typeable     ( TypeRep, Typeable )
import GHC.Generics      ( Generic )
import GHC.Num           ( Num )
import GHC.Stack         ( CallStack, HasCallStack, callStack )
import Text.Show         ( Show(show) )

-- base-unicode-symbols ----------------

import Data.Function.Unicode ( (∘) )
import Prelude.Unicode       ( ℤ )

-- data-textual ------------------------

import Data.Textual ( Printable(print), toText )

-- deepseq -----------------------------

import Control.DeepSeq ( NFData )

-- has-callstack -----------------------

import HasCallstack ( HasCallstack(callstack) )

-- lens --------------------------------

import Control.Lens.Lens   ( Lens', lens )
import Control.Lens.Prism  ( Prism' )
import Control.Lens.Review ( (#) )

-- more-unicode ------------------------

import Data.MoreUnicode.Lens ( (⊣) )

-- mtl ---------------------------------

import Control.Monad.Except ( MonadError, throwError )

-- text-printer ------------------------

import Text.Printer qualified as P

--------------------------------------------------------------------------------

data BoundedErrorType ν = LowerBound { _repType :: TypeRep
                                     , _value   :: ℤ
                                     , _bound   :: ν
                                     }
                        | UpperBound { _repType :: TypeRep
                                     , _value   :: ℤ
                                     , _bound   :: ν
                                     }
  deriving (Eq, Generic, NFData, Show)

repType ∷ Lens' (BoundedErrorType ν) TypeRep
repType =
  lens (\ e → case e of
           LowerBound {} → _repType e
           UpperBound {} → _repType e
       )
       (\ e t → case e of
                  LowerBound {} → e { _repType = t }
                  UpperBound {} → e { _repType = t }
       )

value ∷ Lens' (BoundedErrorType ν) ℤ
value =
  lens (\ e → case e of
           LowerBound {} → _value e
           UpperBound {} → _value e
       )
       (\ e v → case e of
                  LowerBound {} → e { _value = v }
                  UpperBound {} → e { _value = v }
       )

class HasBound γ ν | γ → ν where
  bound ∷ γ → ν

instance HasBound (BoundedErrorType ν) ν where
  bound e@(UpperBound {}) = _bound e
  bound e@(LowerBound {}) = _bound e

--------------------

instance Show ν ⇒ Printable (BoundedErrorType ν) where
  print e@(LowerBound {}) =
    P.string $ unwords [ "value", show (e ⊣ value), "is lower than bound"
                       , show (bound e), "for type", show (e ⊣ repType) ]
  print e@(UpperBound {}) =
    P.string $ unwords [ "value", show (e ⊣ value), "is greater than bound"
                       , show (bound e), "for type", show (e ⊣ repType) ]

------------------------------------------------------------

{-| an error in cmdline calling args & options -}
data BoundedError ν = BoundedError { _boundE    :: BoundedErrorType ν
                                   , _callstack :: CallStack
                                   }
  deriving (Generic, NFData, Show)

----------------------------------------

instance (Typeable ν, Show ν) ⇒ Exception (BoundedError ν)

----------------------------------------

instance Eq ν ⇒ Eq (BoundedError ν) where
  (BoundedError a _) == (BoundedError b _) = a == b

----------------------------------------

instance HasCallstack (BoundedError ν) where
  callstack = lens _callstack (\ eu cs → eu { _callstack = cs })

----------------------------------------

instance Num ν ⇒ HasBound (BoundedError ν) ν where
  bound = bound ∘ _boundE

----------------------------------------

{-| prisms including @BoundedError -}
class AsBoundedError ε ν | ε → ν where
  _BoundedError ∷ Prism' ε (BoundedError ν)

--------------------

instance AsBoundedError (BoundedError ν) ν where
  _BoundedError = id

--------------------

instance Show ν ⇒ Printable (BoundedError ν) where
  print = P.text ∘ toText ∘ _boundE

------------------------------------------------------------

data BEType = UpperBoundType | LowerBoundType

boundedErrorType ∷ BoundedError ν → BEType
boundedErrorType (BoundedError (UpperBound {}) _) = UpperBoundType
boundedErrorType (BoundedError (LowerBound {}) _) = LowerBoundType

{-| create an @AsBoundedError@ for a value that may not be negative -}

lowerBoundError ∷ (AsBoundedError ε ν, HasCallStack) ⇒ TypeRep → ℤ → ν → ε
lowerBoundError t v min =
  let e = LowerBound { _repType = t, _value = v, _bound = min }
  in _BoundedError # BoundedError e callStack

upperBoundError ∷ (AsBoundedError ε ν, HasCallStack) ⇒ TypeRep → ℤ → ν → ε
upperBoundError t v max =
  let e = UpperBound { _repType = t, _value = v, _bound = max }
  in _BoundedError # BoundedError e callStack

----------------------------------------

{-| throw an @AsBoundedError@ -}

throwLowerBoundError ∷ ∀ ε ω ν η . (AsBoundedError ε ν, MonadError ε η) ⇒
                       TypeRep → ℤ → ν → η ω
throwLowerBoundError t v min = throwError $ lowerBoundError t v min

throwUpperBoundError ∷ ∀ ε ω ν η . (AsBoundedError ε ν, MonadError ε η) ⇒
                       TypeRep → ℤ → ν → η ω
throwUpperBoundError t v max = throwError $ upperBoundError t v max

-- that's all, folks! ----------------------------------------------------------
