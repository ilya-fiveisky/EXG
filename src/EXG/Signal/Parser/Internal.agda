module EXG.Signal.Parser.Internal where

open import Data.BoundedLIFO
open import Data.List using (List; []; _∷_; map; mapM; replicateM)
open import Data.Nat
open import Data.Vec using (Vec; fromList; toList; transpose) renaming (map to vmap)
open import EXG.Signal
open import EXG.Signal.Channel
open import EXG.Signal.Sample
open import Function


convert-samples-to-signal : ∀ {n m} → (history-length : ℕ) → (samples : Vec (Sample ℕ n) m) → Signal ℕ n
convert-samples-to-signal hl ss = 
  record
  {
    channels = 
      (vmap (λ x →
        record
        {
          memory-length = hl;
          values = put (toList x) (empty hl)
        }
      ) ∘ transpose ∘ vmap Sample.values) ss
  }
