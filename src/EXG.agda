module EXG where

open import Data.String
open import Data.Unit
open import IO
import IO.Primitive as Prim

main : Prim.IO ⊤
main = Prim.return tt
