module EXG where

open import Data.String
open import Data.Unit
open import IO
import IO.Primitive as Prim
import Network.Primitive as NetPrim
import Control.Exception.Primitive as ExPrim

main : Prim.IO ⊤
main = NetPrim.withSocketsDo
  (ExPrim.bracket (Prim.return tt) (λ _ →  Prim.return tt) (λ _ →  Prim.return tt))
