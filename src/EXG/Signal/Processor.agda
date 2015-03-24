open import Category.Monad

module EXG.Signal.Processor {m : Set → Set}(M : RawMonad m) where

open RawMonad M
open import Data.String
open import Data.Unit

startProcess : m Costring → m ⊤
startProcess _ = return tt
