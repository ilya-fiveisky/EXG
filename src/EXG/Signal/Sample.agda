module EXG.Signal.Sample where

open import Data.Nat
open import Data.Vec

record Sample {l} (A : Set l) (n : ℕ) : Set l where
  field
    values : Vec A n
