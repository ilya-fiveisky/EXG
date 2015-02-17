module EXG where

open import Data.String
open import Data.Unit
open import IO
import IO.Primitive as Prim
open import Network
open import Control.Exception

main : Prim.IO ⊤
main = run (withSocketsDo (bracket (return tt) (λ _ →  return tt) (λ _ →  return tt)))
