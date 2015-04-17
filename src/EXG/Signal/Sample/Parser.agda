module EXG.Signal.Sample.Parser where

open import Data.Bool hiding (_≟_)
import      Data.BoundedVec.Inefficient as BVI
open import Data.Colist hiding (fromList; length; replicate)
open import Data.List hiding (replicate; take)
open import Data.Nat
open import Data.String hiding (_≟_; show)
open import Data.Sum
open import Data.Vec hiding (fromList; take)
open import EXG.Signal.Sample
open import Function

parse : (sample-string-max-length : ℕ) → (expected-channel-count : ℕ) → Costring → String ⊎ (Sample ℕ expected-channel-count)
parse _ _ [] = inj₁ "Empty sample string"
parse ssml n scs = inj₂ $ record {number = 0; values = replicate 0}
  where
  sample-char-list = (BVI.toList ∘ take ssml) scs
  sample-string = fromList sample-char-list


