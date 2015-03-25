open import Category.Monad

module EXG.Signal.Processor {M : Set → Set}(Mon : RawMonad M) where

open import Data.List
open import Data.Nat
open import Data.String
open import Data.Unit
open RawMonad Mon

process : ℕ → M Costring → (String → M ⊤) → M ⊤
process zero _ _ = return tt
process (suc n) input logger = 
  replicateM Mon 256 input >> 
  process n input logger

startProcess : M Costring → (String → M ⊤) → M ⊤
startProcess input logger = process 10 input logger
