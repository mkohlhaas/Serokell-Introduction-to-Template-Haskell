-- separate file due to staging

{-# LANGUAGE TemplateHaskell #-}

module TH where

import Language.Haskell.TH

-- just a normal run of the mill expression
-- >>> :t (1,2)
-- (1,2) ∷ (Num a, Num b) ⇒ (a, b)
--
-- convert our normal expression to AST packed into Q monad
-- >>> :t [| (1,2) |]
-- [| (1,2) |] ∷ ExpQ
--
-- >>> :i ExpQ
-- type ExpQ ∷ *
-- type ExpQ = Q Exp
--
-- >>> :t runQ
-- runQ ∷ Quasi m ⇒ Q a → m a
--
-- extract our AST and show/print it
-- >>> runQ [| (1,2) |]
-- TupE [Just (LitE (IntegerL 1)),Just (LitE (IntegerL 2))]

compose ∷ (t1 → t2) → (t3 → t1) → t3 → t2
compose left right x = left (right x)

-- 2nd step of compilation process.
composeTH ∷ ExpQ
composeTH = [| \left right x → left (right x) |]

-- 2nd step of compilation process.
composeTH' ∷ ExpQ
composeTH' = [| \left right → left (right x) |]

mypair ∷ ExpQ
mypair = [| (1, 2) |]
