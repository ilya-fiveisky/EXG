open import Category.Monad
open import Category.Monad.State
open import EXG.Signal.Processor.Config

module EXG.Signal.Processor 
  {M : Set → Set}{MonadInterpretation : RawMonad M}
  {C : Set}{ConfigInterpretation : Config C}
  where

open import Data.BoundedLIFO using (empty)
import      Data.BoundedVec.Inefficient as BVI
open import Data.Colist hiding (fromList; replicate)
open import Data.List hiding (replicate; take)
open import Data.Nat
open import Data.Nat.Show
open import Data.Product
open import Data.String hiding (show)
open import Data.Vec using (replicate)
open import Data.Unit
import      EXG.Signal.Parser
  {M} {MonadInterpretation}
  {C} {ConfigInterpretation}
  as SP
import      EXG.Signal.Processor.State as PS
open import Function
open        Config ConfigInterpretation

instance MI = MonadInterpretation

process : ∀ {A} → 
  (config : C) →
  {SI : PS.State {channel-count = channel-count config} A ℕ} →
  (recursion-counter : ℕ) →
  (input : M Costring) →
  (logger : String → M ⊤) →
  StateT A M ⊤
process c zero _ logger = lift (logger "zero")
process {A = A} c {SI = SI} (suc n) input logger =
  get 
  >>= λ s →
  lift (SP.parse c input logger)
  >>= λ signal → 
  put (signal →signal-history s)
  >>
  process c {SI} n input logger
  where
  open PS.State SI
  open RawMonadState (StateTMonadState A MI)

startProcess : C → M Costring → (String → M ⊤) → M ⊤
startProcess c input logger =
  logger "start"
  >>
  evalStateT
    (
    process
      c
      {PS.stateImpl}
      (step-count c)
      input
      logger
    )
    record
    {
      signal-history =
        record
        {
          channels = replicate record {values = empty 0}
        }
    }
  where
  open RawMonad MI

