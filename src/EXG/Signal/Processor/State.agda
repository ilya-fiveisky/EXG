module EXG.Signal.Processor.State where

open import Data.Nat using (ℕ)
open import EXG.Signal
open import Level

record State {l l'} {channel-count : ℕ} (A : Set l) (B : Set l') : Set (l ⊔ l') where
  field
    signal-history : A → Signal B channel-count
    _→signal-history_ : Signal B channel-count → A → A

record StateData {l} {n : ℕ} (B : Set l) : Set l where
  field
    signal-history : Signal B n

stateImpl : ∀ {l} {n : ℕ} {B : Set l} → State {_} {_} {n} (StateData B) B
stateImpl = 
    record
    {
      signal-history = StateData.signal-history;
      _→signal-history_ = λ signal sd →
        record sd
        {
          signal-history = put signal into (StateData.signal-history sd)
        }
    }
