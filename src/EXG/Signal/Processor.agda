open import Category.Monad

module EXG.Signal.Processor {M : Set → Set}(Mon : RawMonad M) where

import      Data.BoundedVec.Inefficient as BVI
open import Data.Colist hiding (fromList)
open import Data.List hiding (take)
open import Data.Nat
open import Data.String
open import Data.Unit
open import Function
open        RawMonad Mon


process : (recursion-counter : ℕ) → (input : M Costring) → (logger : String → M ⊤) → M ⊤
process zero _ _ = return tt
process (suc n) input logger = 
  replicateM Mon 256 input >>= 
  λ xs → return tt >> 
  process n input logger
  
 {-
    (head ∷ _) → (logger $ fromList $ BVI.toList $ take 1000 head) >> process n input logger;
    []       → return tt-}

startProcess : M Costring → (String → M ⊤) → M ⊤
startProcess input logger = process 100 input logger
