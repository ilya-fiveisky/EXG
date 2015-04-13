module EXG.Signal.Sample.Parser where

--open import Data.Colist hiding (fromList)
open import Data.Nat
open import Data.String hiding (show)
open import Data.Sum
open import Data.Vec
open import Function
open import EXG.Signal.Sample

parse : (expected-channel-count : ℕ) → Costring → String ⊎ (Sample ℕ expected-channel-count)
parse n ss = inj₂ $ record {values = replicate 0}


