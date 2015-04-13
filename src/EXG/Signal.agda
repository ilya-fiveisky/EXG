module EXG.Signal where

open import Data.BoundedLIFO
open import Data.Nat
open import Data.Vec
open import EXG.Signal.Channel


record Signal {l} (A : Set l) (n : ℕ) : Set l where
  field
    channels : Vec (Channel A) n
