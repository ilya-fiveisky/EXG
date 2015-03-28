module Data.History where

open import Data.List
open import Data.Nat

data History {l} (A : Set l) : ℕ → Set l where
  history : (n : ℕ) → (l : List A) → (length l ≤ n) → History A n

put : ∀ {l} {A : Set l} → {n : ℕ} → List A → History A n → History A n
put l (history n l' _) = history n (take n lsum) (length﹍take﹍n﹍≤﹍n n lsum)
  where
  lsum = l ++ l'

  length﹍take﹍n﹍≤﹍n : ∀ {A} (n : ℕ) (l : List A) → length (take n l) ≤ n
  length﹍take﹍n﹍≤﹍n zero _ = z≤n
  length﹍take﹍n﹍≤﹍n (suc n) [] = z≤n
  length﹍take﹍n﹍≤﹍n (suc n) (x ∷ xs) = s≤s (length﹍take﹍n﹍≤﹍n n xs)
