open import Category.Monad
open import EXG.Signal.Processor.Config

module EXG.Signal.Parser 
  {M : Set → Set}{MonadInterpretation : RawMonad M}
  {C : Set}{ConfigInterpretation : Config C} where

open        Config ConfigInterpretation
open import Data.Bool
open import Data.BoundedLIFO
--open import Data.Colist hiding (fromList)
open import Data.List hiding (replicate)
open import Data.Nat
open import Data.Nat.Show
open import Data.Product hiding (map)
open import Data.String hiding (show)
open import Data.Sum hiding (map)
open import Data.Unit
open import Data.Vec using (replicate)
open import EXG.Signal
open import EXG.Signal.Channel
open import EXG.Signal.Sample
import      EXG.Signal.Sample.Parser as SP
open import Function
instance MI = MonadInterpretation
open        RawMonad MI

convert-sample-list-to-signal : ∀ {n} → (l : List (Sample ℕ n)) → Signal ℕ n
convert-sample-list-to-signal l = record {channels = replicate (record {memory-length = 0; values = empty 0})}

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

parse : (config : C) → (input : M Costring) → (logger : String → M ⊤) → M (Signal ℕ (channel-count config))
parse c input logger =
  replicateM MI (sampling-rate c) input
  >>= λ l →
    log-errors l
    >>
    (return ∘ convert-sample-list-to-signal ∘ correct-samples) l
  where
  parsed-samples = map (SP.parse (sample-string-max-length c) (channel-count c))
  errors = filter-inj₁ ∘ parsed-samples
  log-errors = (mapM MI logger) ∘ errors
  correct-samples = filter-inj₂ ∘ parsed-samples
