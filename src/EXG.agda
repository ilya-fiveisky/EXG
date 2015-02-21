module EXG where

open import Data.String
open import Data.Unit
open import IO
import IO.Primitive as Prim
open import Network
import Network.Primitive as NetPrim
open import Control.Exception

main : Prim.IO ⊤
main = run (withSocketsDo (bracket (lift (NetPrim.connectTo "localhost" "80")) (λ _ →  return tt) (λ _ →  return tt)))
