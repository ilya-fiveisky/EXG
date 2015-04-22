open import Category.Monad
open import EXG.Signal.Processor.Config

module EXG.Signal.Parser 
  {M : Set → Set}{MonadInterpretation : RawMonad M}
  {C : Set}{ConfigInterpretation : Config C} where

open        Config ConfigInterpretation
open import Data.Bool
open import Data.BoundedLIFO
open import Data.List using (List; []; _∷_; map; mapM; replicateM)
open import Data.Nat
open import Data.Nat.Show
open import Data.Product hiding (map)
open import Data.String hiding (fromList; show; toList)
open import Data.Unit
open import Data.Vec using (Vec; fromList; toList; transpose)
  renaming (map to vmap)
open import EXG.Signal
open import EXG.Signal.Channel
open import EXG.Signal.Parser.Internal
open import EXG.Signal.Sample
import      EXG.Signal.Sample.Parser as SP
open import Function
instance MI = MonadInterpretation
open        RawMonad MI

parse : 
  (config : C) →
  (input : M Costring) →
  (logger : String → M ⊤) →
  M (Signal ℕ (channel-count config))
parse c input logger =
  replicateM MI (sampling-rate c) input
  >>= λ l →
    log-errors l
    >>
    (return ∘ convert-samples-to-signal (sample-history-length c) ∘ correct-samples) l
  where
  parsed-samples = map (SP.parse (sample-string-max-length c) (channel-count c))
  errors = filter-inj₁ ∘ parsed-samples
  log-errors = (mapM MI logger) ∘ errors
  correct-samples = fromList ∘ filter-inj₂ ∘ parsed-samples
