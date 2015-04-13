module EXG.Signal.Processor.State where

open import Data.Nat
open import EXG.Signal

record State {l} (A : Set l) : Set l where
  field
    signal-history : ∀ {n} → A → Signal A n
