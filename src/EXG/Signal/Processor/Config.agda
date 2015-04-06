module EXG.Signal.Processor.Config where

open import Data.Nat hiding (suc)
open import Level

record Config {l} (A : Set l) : Set l where
  field
    sampling-rate : A → ℕ
    step-count : A → ℕ