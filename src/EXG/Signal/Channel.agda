module EXG.Signal.Channel where

open import Data.BoundedLIFO
open import Data.Nat


record Channel {l} (A : Set l) : Set l where
  field
    {memory-length} : ℕ
    values : BoundedLIFO A memory-length
