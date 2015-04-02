open import Category.Monad
open import EXG.Signal.Processor.Config

module EXG.Signal.Processor 
  {M : Set → Set}{MonadInterpretation : RawMonad M}
  {C : Set}{ConfigInterpretation : Config C} where

import      Data.BoundedVec.Inefficient as BVI
open import Data.Colist hiding (fromList)
open import Data.List hiding (take)
open import Data.Nat
open import Data.String
open import Data.Unit
open import Function
open        RawMonad MonadInterpretation
open        Config ConfigInterpretation


process : (config : C) → (recursion-counter : ℕ) → (input : M Costring) → (logger : String → M ⊤) → M ⊤
process c zero _ _ = return tt
process c (suc n) input logger = 
  replicateM MonadInterpretation (sampling-rate c) input >>= 
--  λ xs → return tt >> 
--  process c n input logger
    λ {(head ∷ _) → (logger $ fromList $ BVI.toList $ take 1000 head) >> process c n input logger;
    []       → return tt}

startProcess : C → M Costring → (String → M ⊤) → M ⊤
startProcess c input logger = process c (step-count c) input logger
