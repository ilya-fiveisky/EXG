open import Category.Monad
open import Category.Monad.State
open import EXG.Signal.Processor.Config

module EXG.Signal.Processor 
  {M : Set → Set}{MonadInterpretation : RawMonad M}
  {C : Set}{ConfigInterpretation : Config C} where

import      Data.BoundedVec.Inefficient as BVI
open import Data.Colist hiding (fromList)
open import Data.List hiding (take)
open import Data.Nat
open import Data.Product
open import Data.String
open import Data.Unit
open import Function
open        RawMonad MonadInterpretation using () renaming (_>>=_ to _>>='_; return to return'; _>>_ to _>>'_)
open        Config ConfigInterpretation


instance MI = MonadInterpretation

process : (config : C) → (recursion-counter : ℕ) → (input : M Costring) → (logger : String → M ⊤) → StateT ℕ M ⊤
process c zero _ logger = lift (logger "zero")
  where open RawMonad (StateTMonad ℕ MonadInterpretation)
process c (suc n) input logger =
  lift
  (
      replicateM MonadInterpretation (sampling-rate c) input >>='
    --  λ _ → return tt >> 
    --  process c n input logger
      λ {(head ∷ _) → logger $ fromList $ BVI.toList $ take 1000 head;
      []       → return' tt}
  )
  >>
  process c n input logger
  where open RawMonad (StateTMonad ℕ MonadInterpretation)

startProcess : C → M Costring → (String → M ⊤) → M ⊤
startProcess c input logger = logger "start" >>' evalStateT (process c (step-count c) input logger) 0
