module EXG.Signal where

open import Data.BoundedLIFO renaming (put_into_ to putbl_intobl_)
open import Data.Nat
open import Data.Vec
open import EXG.Signal.Channel


record Signal {l} (A : Set l) (n : ℕ) : Set l where
  field
    channels : Vec (Channel A) n

put_into_ : ∀ {l n} {A : Set l} → Signal A n → Signal A n → Signal A n
put a into b =
  record b
  {
    channels = zipWith
      (λ cha chb → 
        record chb
        {
          values = putbl (Channel.values cha) intobl (Channel.values chb)
        }
      )
      (Signal.channels a)
      (Signal.channels b)
  }
