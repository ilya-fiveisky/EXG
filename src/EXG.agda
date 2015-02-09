module EXG where

open import Data.String
open import Data.Unit
open import IO
import IO.Primitive as Prim
import Network.Primitive as NetPrim

main : Prim.IO ⊤
main = NetPrim.withSocketsDo (Prim.return tt)
