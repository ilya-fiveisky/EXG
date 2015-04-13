module EXG.AppConfig where

open import Data.Nat


record AppConfig : Set where
  field
    channel-count : ℕ
    sampling-rate : ℕ
    step-count : ℕ
