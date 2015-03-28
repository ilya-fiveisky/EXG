module EXG.Signal.Channel where

open import Data.History
open import Data.Nat


record Channel {l} (A : Set l) : Set l where
  field
    memory-length : ℕ
    values : History A memory-length
