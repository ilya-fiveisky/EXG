module Main where

open import Coinduction
open import Category.Monad
open import Data.Bool
import      Data.BoundedVec.Inefficient as BVI
open import Data.Colist using (take)
open import Data.Fin hiding (_<_; lift)
open import Data.List using (length)
open import Data.Nat
open import Data.String
open import Data.Sum
open import Data.Unit
open import Data.Vec hiding (_>>=_; fromList; take; toList)
open import Function
open import IO
import      IO.Primitive as Prim
open import Network
import      Network.Primitive as NetPrim
open import Control.Exception

open import EXG.AppConfig
open import EXG.Signal.Processor 
  {MonadInterpretation = record {return = Prim.return; _>>=_ = Prim._>>=_}}
  {ConfigInterpretation = record {sampling-rate = AppConfig.sampling-rate; step-count = AppConfig.step-count}}


isResponseOk : Prim.Handle → IO (String ⊎ ⊤)
isResponseOk h = ♯ hGetLine h >>= λ r → 
  ♯ let response = responseToString r in 
        if response == okStr then return (inj₂ tt) else return (inj₁ response)
  where
    okStr = "200 OK\r"
    maxResponseLength = 100 --length (toList okStr)
    responseToString : Costring → String
    responseToString r = fromList (BVI.toList (take maxResponseLength r))

send : Prim.Handle → String → IO ⊤
send h command = hPutStrLn h command

setupConnection : Prim.Handle → IO (String ⊎ ⊤)
setupConnection h =
  ♯ hSetBuffering h Prim.LineBuffering >> ♯ (
  ♯ send h "display" >> ♯ (
  ♯ isResponseOk h >>= λ {
    (inj₂ _) → ♯ (
      ♯ send h "watch" >> ♯ (
      ♯ isResponseOk h >>= λ {
        (inj₂ _) → ♯ return (inj₂ tt);
        (inj₁ watchResponse) → ♯ return (inj₁ watchResponse)}));
    (inj₁ displayResponse) → ♯ return (inj₁ displayResponse)}))

app-config : AppConfig
app-config = record {
  sampling-rate = 256;
  step-count = 5}

main : Prim.IO ⊤
main = run $ withSocketsDo $ bracket (connectTo (IPv4 ((# 127) ∷ (# 0) ∷ (# 0) ∷ (# 1) ∷ [])) (portNum (# 8336))) hClose (λ h → 
  ♯ setupConnection h >>= λ {
    (inj₁ error) → ♯ putStrLn error;
    (inj₂ _) → ♯ lift (startProcess app-config (run (hGetLine h)) (λ s → run (putStrLn s))) }
  )
