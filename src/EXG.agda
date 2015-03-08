module EXG where

open import Coinduction
open import Data.Bool
import Data.BoundedVec.Inefficient as BVI
open import Data.Colist using (take)
open import Data.Fin hiding (_<_)
open import Data.List using (length)
open import Data.Nat
open import Data.String
open import Data.Sum
open import Data.Unit
open import Data.Vec hiding (_>>=_; fromList; take; toList)
open import Function
open import IO
import IO.Primitive as Prim
open import Network
import Network.Primitive as NetPrim
open import Control.Exception

isResponseOk : Prim.Handle → IO (String ⊎ ⊤)
isResponseOk h = ♯ (hGetLine h) >>= λ r → 
  ♯ (let response = responseToString r in 
    if response == okStr then return (inj₂ tt) else return (inj₁ response))
  where
    okStr = "200 OK\r"
    maxResponseLength = length (toList okStr)
    responseToString : Costring → String
    responseToString r = fromList (BVI.toList (take maxResponseLength r))

main : Prim.IO ⊤
main = run $ withSocketsDo $ bracket (connectTo (IPv4 ((# 127) ∷ (# 0) ∷ (# 0) ∷ (# 1) ∷ [])) (portNum (# 8336))) hClose (λ h → hPutStrLn h "display")
