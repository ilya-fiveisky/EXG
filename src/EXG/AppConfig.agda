module EXG.AppConfig where

open import Data.Nat


record AppConfig : Set where
  field
    channel-count : ℕ
    sample-history-length : ℕ
    sample-string-max-length : ℕ
    sampling-rate : ℕ
    step-count : ℕ
