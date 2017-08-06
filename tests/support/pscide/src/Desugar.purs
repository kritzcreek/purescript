module Desugar where

import ImportsSpec1 (MyMaybe(..), MyParamType)

data List a = Cons a (List a) | Nil

filter ∷ ∀ a. (a → Boolean) → List a → List a
filter pred = case _ of
  Nil → Nil
  Cons x xs →
    let rest = filter pred xs
    in if pred x then Cons x rest else rest

f ∷ MyParamType (MyMaybe Int)
f = [MyJust 1]
