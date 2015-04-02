module EXG.AppConfig where

open import Data.Nat


record AppConfig : Set where
  field
    sampling-rate : ℕ
    step-count : ℕ
