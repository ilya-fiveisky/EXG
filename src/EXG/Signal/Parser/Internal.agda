module EXG.Signal.Parser.Internal where

open import Data.BoundedLIFO hiding (toList)
open import Data.List using (List; []; _∷_; map; mapM; replicateM)
open import Data.Nat
open import Data.Sum
open import Data.Vec using (Vec; fromList; toList; transpose)
  renaming (map to vmap)
open import EXG.Signal
open import EXG.Signal.Channel
open import EXG.Signal.Sample
open import Function

convert-samples-to-signal : ∀ {n m} → 
  (history-length : ℕ) →
  (samples : Vec (Sample ℕ n) m) →
  Signal ℕ n
convert-samples-to-signal hl ss = 
  record
  {
    channels = 
      (
        vmap (λ x → record {values = put (toList x) (empty hl) })
        ∘ transpose
        ∘ vmap Sample.values
      )
      ss
  }

filter-inj₁ : ∀ {l} {A B : Set l} → List (A ⊎ B) → List A
filter-inj₁ [] = []
filter-inj₁ (x ∷ xs) with x
... | (inj₁ y) = y ∷ filter-inj₁ xs
... | (inj₂ _) = filter-inj₁ xs

filter-inj₂ : ∀ {l} {A B : Set l} → List (A ⊎ B) → List B
filter-inj₂ [] = []
filter-inj₂ (x ∷ xs) with x
... | (inj₁ _) = filter-inj₂ xs
... | (inj₂ y) = y ∷ filter-inj₂ xs
