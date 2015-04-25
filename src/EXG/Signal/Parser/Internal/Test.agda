module EXG.Signal.Parser.Internal.Test where

open import Data.BoundedLIFO
open import Data.List renaming ([_] to l[_])
open import Data.Nat
open import Data.Vec
open import EXG.Signal
open import EXG.Signal.Channel
open import EXG.Signal.Parser.Internal
open import EXG.Signal.Sample
open import Relation.Binary.PropositionalEquality as PropEq
  using (_≡_; refl)


test₁ :
  convert-samples-to-signal 0 []
  ≡
  record {channels = []}
test₁ = refl

test₂ :
  convert-samples-to-signal 0 (record {number = 0; values = [ 0 ]} ∷ [])
  ≡
  record {channels = [ record {values = empty 0} ]}
test₂ = refl

test₃ : 
  convert-samples-to-signal 1 (record {number = 0; values = [ 2 ]} ∷ [])
  ≡
  record {channels = [ record {values = put l[ 2 ] (empty 1)} ]}
test₃ = refl

test₄ : 
  convert-samples-to-signal 1 (record {number = 0; values = 2 ∷ 3 ∷ []} ∷ [])
  ≡
  record {channels =
    (record {values = put l[ 2 ] (empty 1)}) ∷
    (record {values = put l[ 3 ] (empty 1)})  ∷ [] }
test₄ = refl
