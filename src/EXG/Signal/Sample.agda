module EXG.Signal.Sample where

open import Data.Nat
open import Data.Vec

record Sample {l} (A : Set l) (n : ℕ) : Set l where
  field
    number : ℕ
    values : Vec A n
