module EXG where

open import Data.Fin hiding (_<_)
open import Data.Nat
open import Data.String
open import Data.Unit
open import Data.Vec
open import Function
open import IO
import IO.Primitive as Prim
open import Network
import Network.Primitive as NetPrim
open import Control.Exception

main : Prim.IO ⊤
main = run $ withSocketsDo $ bracket (connectTo (IPv4 ((# 127) ∷ (# 0) ∷ (# 0) ∷ (# 1) ∷ [])) (portNum (# 8336))) hClose (λ h → hPutStrLn h "display")
