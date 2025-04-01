{-# LANGUAGE UnicodeSyntax #-}
module Natural.T.Natural
  ( tests
  ) where

import Base0T  hiding ( (⊕) )
import Prelude ( ($!) )

-- base --------------------------------

import Control.Exception ( ErrorCall, catch )
import Data.Either       ( isLeft, isRight )
import Data.Int          ( Int32, Int64 )
import Data.Ord          ( Ordering(EQ, GT, LT) )
import Data.Typeable     ( typeOf )
import GHC.Enum          ( maxBound )
import GHC.Exts          ( Int )
import GHC.Num           ( (*) )
import GHC.Real          ( Integral )

-- base-unicode-symbols ----------------

import Prelude.Unicode ( (×) )

-- bytestring --------------------------

import Data.ByteString      qualified as BS
import Data.ByteString.Lazy qualified as BSL

-- more-unicode ------------------------

import Data.MoreUnicode.Either    ( 𝔼, pattern 𝕷, pattern 𝕽 )
import Data.MoreUnicode.Maybe     ( pattern 𝕵 )
import Data.MoreUnicode.Monad     ( (≫) )
import Data.MoreUnicode.Monoid    ( ю )
import Data.MoreUnicode.Semigroup ( (◇) )
import Data.MoreUnicode.String    ( 𝕊 )
import Data.MoreUnicode.Text      ( 𝕋 )

-- tasty -------------------------------

import Test.Tasty ( defaultMain )

-- tasty-hunit -------------------------

import Test.Tasty.HUnit ( Assertion, assertBool )

-- tasty-quickcheck --------------------

import Test.Tasty.QuickCheck ( testProperty )

-- text --------------------------------

import Data.Text      qualified as Text
import Data.Text.Lazy qualified as LazyText

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import Natural              ( I64, Length(len, len_, length),
                              Unsigned(boundMax', fromI, fromI', fromI0),
                              fromEnum, fromEnum_, propOpRespectsBounds,
                              replicate, replicate_, toEnum, toEnum', toEnum_,
                              (⨹), (⨺), (⨻) )
import Natural.BoundedError ( BoundedError )


--------------------------------------------------------------------------------

lengthTests ∷ TestTree
lengthTests = testGroup "length" $
  let longString ∷ 𝕊 = ю [ "The quick brown fox jumped over the lazy dog"
                         , "The quick brown fox jumped over the lazy dog"
                         , "The quick brown fox jumped over the lazy dog"
                         , "The quick brown fox jumped over the lazy dog"
                         , "The quick brown fox jumped over the lazy dog"
                         , "The quick brown fox jumped over the lazy dog" ]
  in [ testCase "𝕊 → W64  6 len"    $ (  6 ∷ Word64) @=? len_ ("foobar" ∷ 𝕊)
     , testCase "𝕊 → ℕ  264 len"    $ (264 ∷ ℕ) @=? length longString
     , testCase "𝕊 → ℕ    6 length" $   6 @=? length ("foobar" ∷ 𝕊)
     , testCase "𝕊 → ℕ  264 length" $ 264 @=? length longString
     , testCase "𝕋 → ℕ    6"        $ (  6 ∷ ℕ) @=? length ("foobar" ∷ 𝕋)
     ]

----------------------------------------

fromITests ∷ TestTree
fromITests = testGroup "fromI*" $
  let showT ∷ (Typeable γ, Show γ) ⇒ γ → 𝕊
      showT x = show x ◇ "∷" ◇ show (typeOf x)

      -- check that `fromI` of `input` matches predicate `p`
      check__ ∷ (Typeable β, Integral β, Show β) ⇒
                (β → γ) → (γ → Assertion) → β → 𝕊
              → TestTree
      check__ f p input exp =
        let name = showT input ◇ " → " ◇ exp
        in  testCase name $ p (f input)

      -- check that `fromI` of `input` matches predicate `p`
      checkI_ ∷ (Integral α, Unsigned α, -- Typeable α, Eq α, Show α,
                 Typeable β, Integral β, Show β) ⇒
                (𝔼 (BoundedError α) α → Assertion) → β → 𝕊 → TestTree
      checkI_ = check__ fromI

      -- check that `fromI0` of `input` matches predicate `p`
      checkI0_ ∷ (Integral α, Unsigned α, Typeable α, Eq α, Show α,
                 Typeable β, Integral β, Show β) ⇒
                (𝔼 (BoundedError α) α → Assertion) → β → 𝕊 → TestTree
      checkI0_ = check__ fromI0

      -- check that `fromI0` of `input` matches predicate `p`
      checkI'_ ∷ (Integral α, Unsigned α, Typeable α, Eq α, Show α,
                 Typeable β, Integral β, Show β) ⇒
                (α → Assertion) → β → 𝕊 → TestTree
      checkI'_ = check__ fromI'

      -- check that `fromI` of `input` matches value `expect`
      checkI ∷ ∀ α β . (Integral α, Unsigned α, Typeable α, Eq α, Show α,
                        Typeable β, Integral β, Show β) ⇒ β → α → TestTree
      checkI input expect = checkI_ (𝕽 expect @=?) input (showT expect)

      -- check that `fromI0` of `input` matches value `expect`
      checkI0 ∷ ∀ α β . (Integral α, Unsigned α, Typeable α, Eq α, Show α,
                        Typeable β, Integral β, Show β) ⇒ β → α → TestTree
      checkI0 input expect = checkI0_ (𝕽 expect @=?) input (showT expect)

      -- check that `fromI'` of `input` matches value `expect`
      checkI' ∷ ∀ α β . (Integral α, Unsigned α, Typeable α, Eq α, Show α,
                        Typeable β, Integral β, Show β) ⇒ β → α → TestTree
      checkI' input expect = checkI'_ (expect @=?) input (showT expect)

      -- check that `fromI` of `input` throws an error with text `expect`
      checkIE ∷ (Typeable β, Integral β, Show β, Integral α,Unsigned α,Show α)⇒
                (α → 𝕋) → β → 𝕋 → TestTree
      checkIE s input expect =
        checkI_ (\ x → expect @=? either toText s x) input (show expect)

      -- check that `fromI` of `input` throws an error with text `expect`
      checkI0E ∷ (Typeable β, Integral β, Show β,Integral α,Unsigned α,Show α)⇒
                 (α → 𝕋) → β → 𝕋 → TestTree
      checkI0E s input expect =
        checkI0_ (\ x → expect @=? either toText s x) input (show expect)

  in  [ testGroup "Natural" $
          [ testGroup "fromI" $
              let chk ∷  ∀ β . (Typeable β,Integral β,Show β) ⇒ β→ℕ→TestTree
                  chk = checkI
                  eek ∷ (Typeable β, Integral β, Show β) ⇒ β → 𝕋 → TestTree
                  eek = checkIE (Text.pack ∘ show @ℕ)
              in  [ chk (3 ∷ ℕ) 3
                  , chk (3 ∷ ℤ) 3
                  , chk (3 ∷ Int) 3
                  , chk (3 ∷ Word16) 3
                  , chk (0 ∷ Word32) 0
                  , chk (0 ∷ ℕ) 0
                  , eek (-1 ∷ ℤ)
                    "value -1 is lower than bound 0 for type Natural"
                  , eek (-1 ∷ Int32)
                    "value -1 is lower than bound 0 for type Natural"
                  ]

          , testGroup "fromI0" $
              let chk ∷  ∀ β . (Typeable β,Integral β,Show β) ⇒ β→ℕ→TestTree
                  chk = checkI0
              in  [ chk ( 3 ∷ ℕ     ) 3
                  , chk ( 3 ∷ ℤ     ) 3
                  , chk ( 3 ∷ Int   ) 3
                  , chk ( 3 ∷ Word16) 3
                  , chk ( 0 ∷ Word32) 0
                  , chk ( 0 ∷ ℕ     ) 0
                  , chk (-1 ∷ ℤ     ) 0
                  , chk (-1 ∷ Int32 ) 0
                  ]
          ]

      , testGroup "Word8" $
          [ testGroup "fromI" $
              let chk ∷  ∀ β . (Typeable β,Integral β,Show β)⇒ β→Word8→TestTree
                  chk = checkI
                  eek ∷ (Typeable β, Integral β, Show β) ⇒ β → 𝕋 → TestTree
                  eek = checkIE (Text.pack ∘ show @Word8)
              in  [ chk (  3 ∷ ℕ) 3
                  , chk (  3 ∷ ℤ) 3
                  , chk (  3 ∷ Int) 3
                  , chk (  3 ∷ Word16) 3
                  , chk (  0 ∷ Word32) 0
                  , chk (  0 ∷ Word8) 0
                  , eek ( -1 ∷ ℤ)
                    "value -1 is lower than bound 0 for type Word8"
                  , eek ( -1 ∷ Int32)
                    "value -1 is lower than bound 0 for type Word8"
                  , eek (256 ∷ ℕ)
                    "value 256 is greater than bound 255 for type Word8"
                  ]

          , testGroup "fromI0" $
              let chk ∷  ∀ β . (Typeable β,Integral β,Show β)⇒ β→Word8→TestTree
                  chk = checkI0
                  eek ∷ (Typeable β, Integral β, Show β) ⇒ β → 𝕋 → TestTree
                  eek = checkI0E (Text.pack ∘ show @Word8)
              in  [ chk ( 3 ∷ Word8     ) 3
                  , chk ( 3 ∷ ℤ     ) 3
                  , chk ( 3 ∷ Int   ) 3
                  , chk ( 3 ∷ Word16) 3
                  , chk ( 0 ∷ Word32) 0
                  , chk ( 0 ∷ ℕ     ) 0
                  , chk (-1 ∷ ℤ     ) 0
                  , chk (-1 ∷ Int32 ) 0
                  , eek (256 ∷ ℕ)
                    "value 256 is greater than bound 255 for type Word8"
                  ]

          , testGroup "fromI'" $
              let chk ∷  ∀ β . (Typeable β,Integral β,Show β)⇒ β→Word8→TestTree
                  chk = checkI'
              in  [ chk ( 3  ∷ ℕ     ) 3
                  , chk ( 3  ∷ ℤ     ) 3
                  , chk ( 3  ∷ Int   ) 3
                  , chk ( 3  ∷ Word16) 3
                  , chk ( 0  ∷ Word32) 0
                  , chk ( 0  ∷ ℕ     ) 0
                  , chk (-1  ∷ ℤ     ) 0
                  , chk (-1  ∷ Int32 ) 0
                  , chk (256 ∷ ℕ     ) 255
                  ]
          ]

      ]

----------------------------------------

replicateTests ∷ TestTree
replicateTests = testGroup "replicate" $
  let 𝕵 maxI64 = boundMax' (0∷I64)
  in [ testCase "𝕊 3" $ 𝕽 "ccc" @=? replicate @𝕊 @_ @(BoundedError I64) 3 'c'
     , testCase "𝕊 max@I64" $
       assertBool "should be 𝕽" ∘ isRight $
       replicate @𝕊 @_ @(BoundedError I64) maxI64 'c'
     , testCase "𝕊 1+max@I64" $
       assertBool "should be 𝕷" ∘ isLeft $
       replicate @𝕊 @_ @(BoundedError I64) (1+maxI64) 'c'
     , testCase "𝕋   3" $ "ccc" @=? replicate_ @𝕋 3 'c'
     , testCase "𝕃𝕋  3" $ "ccc" @=? replicate_ @LazyText.Text 3 'c'
     , testCase "BS  3" $ "ccc" @=? replicate_ @BS.ByteString 3 99
     , testCase "BSL 3" $ "ccc" @=? replicate_ @BSL.ByteString 3 99
     ]

----------------------------------------

enumTests ∷ TestTree
enumTests = testGroup "enum" $
  let from'Enum  = fromEnum @(BoundedError _) @_ @_ @(𝔼 _)
      from'Enum_ = fromEnum_ @_ @Word8
      to'Enum    = toEnum @(BoundedError _) @Word8 @_ @(𝔼 _)
      to'Enum'   = toEnum' @(BoundedError _) @ℕ @_ @(𝔼 _)
      to'Enum_   = toEnum_ @Word8
      ii ∷ ℕ     = fromIntegral $ maxBound @Int64
      -- ($!) is variant of ($) that forces its argument to WHNF
      -- this is necessary to force the error to be evaluated
      catch_error_call f =
        catch (return $! 𝕽 $! f) (\ (e ∷ ErrorCall) -> return $ 𝕷 e)
  in  [ testCase "fromEnum" $ 𝕽 (1 ∷ Word8) @=? from'Enum EQ
      , testCase "fromEnum (error)" $
          assertBool "should be 𝕷" ∘ isLeft $
            fromEnum @(BoundedError _) @_ @Word8 @(𝔼 _) 256
      , testCase "fromEnum_" $ 0 @=? from'Enum_ LT
--ERROR CASE?
      , testCase "toEnum"    $ 𝕽 GT @=? to'Enum 2
      , testCase "toEnum (error)" $
          assertBool "should be 𝕷" ∘ isLeft $
            toEnum @(BoundedError _) @Word8 @Ordering @(𝔼 _) 3
      , testCase "toEnum'"   $ 𝕽 ii @=? to'Enum' ii
      , testCase "toEnum_"   $ EQ @=? to'Enum_ 1
--ERROR CASE?
      , testCase "toEnum_ (error)" $
          (catch_error_call $ toEnum_ @Word8 @Ordering 3) ≫
            (assertBool "should be 𝕷" ∘ isLeft)
      ]

----------------------------------------

operatorTests ∷ TestTree
operatorTests = testGroup "operators" $
  [ testCase "Word8 2 + 2" $ 4 @=? (2∷Word8) + 2
  , testCase "Word8 255 + 2" $ 1 @=? (255∷Word8) + 2
  , testCase "Word8 1 - 2" $ 255 @=? (1∷Word8) - 2
  , testCase "Word8 150 * 2" $ 44 @=? (150∷Word8) * 2
  , testCase "Word8 100 × 3" $ 44 @=? (100∷Word8) × 3
  , testCase "Word8 2 ⨹ 2" $ 𝕽 4 @=? (2∷Word8) ⨹ 2
  , testCase "Word8 255 ⨹ 2" $
      assertBool "should be 𝕷" ∘ isLeft $ (255∷Word8) ⨹ 2
  , testCase "Word8 2 ⨺ 1" $ 𝕽 1 @=? (2∷Word8) ⨺ 1
  , testCase "Word8 1 ⨺ 2" $
      assertBool "should be 𝕷" ∘ isLeft $ (1∷Word8) ⨺ 2
  , testCase "Word8 29 ⨻ 9" $
      assertBool "should be 𝕷" ∘ isLeft $ (29∷Word8) ⨻ 9
  , testProperty "⨹ bounds check (W8)" (propOpRespectsBounds @Word8 (⨹) (+))
  , testProperty "⨹ bounds check (W64)" (propOpRespectsBounds @Word64 (⨹) (+))
  , testProperty "⨺ bounds check (W8)" (propOpRespectsBounds @Word8 (⨺) (-))
  , testProperty "⨺ bounds check (W64)" (propOpRespectsBounds @Word64 (⨺) (-))
  , testProperty "⨻ bounds check (W8)" (propOpRespectsBounds @Word8 (⨻) (×))
  , testProperty "⨻ bounds check (W64)" (propOpRespectsBounds @Word64 (⨻) (×))
  ]

-- tests -----------------------------------------------------------------------

-- | run the tests
_test ∷ IO ()
_test = defaultMain tests

tests ∷ TestTree
tests = testGroup "Natural" [ lengthTests, replicateTests, fromITests, enumTests
                            , operatorTests ]

-- that's all, folks! ----------------------------------------------------------
