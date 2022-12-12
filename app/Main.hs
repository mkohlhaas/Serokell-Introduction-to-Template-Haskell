{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -ddump-splices #-}

module Main where

import Language.Haskell.TH
import TH
import TuplesTH

-- just a normal run-of-the-mill expression
-- >>> :t (1,2)
-- (1,2) ∷ (Num a, Num b) ⇒ (a, b)
--
-- convert our normal expression to AST packed into the Q monad
-- >>> :t [| (1,2) |]
-- [| (1,2) |] ∷ ExpQ
--
-- >>> :i ExpQ
-- type ExpQ ∷ *
-- type ExpQ = Q Exp
--   	-- Defined in ‘Language.Haskell.TH.Lib.Internal’
--
-- >>> :t runQ
-- runQ ∷ Quasi m ⇒ Q a → m a
--
-- extract our AST expression (`Exp`) from Q monad put it into IO monad and show/print it
-- runQ ∷ Quasi m ⇒ Q Exp → IO Exp
-- >>> runQ [| (1,2) |]
-- TupE [Just (LitE (IntegerL 1)),Just (LitE (IntegerL 2))]
--
-- 1st and 2nd step of compilation process.
-- >>> compose (+ 1) (+ 2) 5
-- 8
--
-- splicing: generate executable function/code from AST (`Q Exp`)
-- 2nd step of compilation process.
-- >>> $composeTH (+ 1) (+ 2) 5
-- 8
--
-- injecting code
-- >>> $composeTH' (+ 1) (+ 2)
-- 8

x ∷ Integer
x = 5

mp ∷ (Integer, Integer)
mp = $mypair

-- $(generateTupleClass 3)

-- >>> :i Tuple3
-- type Tuple3 ∷ * → * → Constraint
-- class Tuple3 t r | t → r where
--   _3 ∷ t → r
--   {-# MINIMAL _3 #-}
--   	-- Defined at /home/schmidh/Temp/thtest/app/Main.hs:50:3

-- >>> runQ [d|instance Tuple3 (a, b, c) c where _3 (_, _, c) = c|]
-- [InstanceD Nothing [] (AppT (AppT (ConT Main.Tuple3) (AppT (AppT (AppT (TupleT 3) (VarT a_23)) (VarT b_24)) (VarT c_25))) (VarT c_25)) [FunD Main._3 [Clause [TupP [WildP,WildP,VarP c_26]] (NormalB (VarE c_26)) []]]]

-- $(generateTupleInstance 3 5)

-- >>> :i Tuple3
-- type Tuple3 ∷ * → * → Constraint
-- class Tuple3 t r | t → r where
--   _3 ∷ t → r
--   {-# MINIMAL _3 #-}
--   	-- Defined at /home/schmidh/Temp/thtest/app/Main.hs:52:3
-- instance Tuple3 (t1, t2, t3, t4, t5) t3
--   -- Defined at /home/schmidh/Temp/thtest/app/Main.hs:64:3

-- >>> _3 (42, "hello", '#', [], 3.14)
-- '#'

-- 62 elements is the maximum tuple size allowed by GHC.
$(generateTupleBoilerplate 10)

-- >>> :i Tuple3
-- type Tuple3 ∷ * → * → Constraint
-- class Tuple3 t r | t → r where
--   _3 ∷ t → r
--   {-# MINIMAL _3 #-}
--   	-- Defined at /home/schmidh/Temp/thtest/app/Main.hs:79:3
-- instance Tuple3 (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10) t3
--   -- Defined at /home/schmidh/Temp/thtest/app/Main.hs:79:3
-- instance Tuple3 (t1, t2, t3, t4, t5, t6, t7, t8, t9) t3
--   -- Defined at /home/schmidh/Temp/thtest/app/Main.hs:79:3
-- instance Tuple3 (t1, t2, t3, t4, t5, t6, t7, t8) t3
--   -- Defined at /home/schmidh/Temp/thtest/app/Main.hs:79:3
-- instance Tuple3 (t1, t2, t3, t4, t5, t6, t7) t3
--   -- Defined at /home/schmidh/Temp/thtest/app/Main.hs:79:3
-- instance Tuple3 (t1, t2, t3, t4, t5, t6) t3
--   -- Defined at /home/schmidh/Temp/thtest/app/Main.hs:79:3
-- instance Tuple3 (t1, t2, t3, t4, t5) t3
--   -- Defined at /home/schmidh/Temp/thtest/app/Main.hs:79:3
-- instance Tuple3 (t1, t2, t3, t4) t3
--   -- Defined at /home/schmidh/Temp/thtest/app/Main.hs:79:3
-- instance Tuple3 (t1, t2, t3) t3
--   -- Defined at /home/schmidh/Temp/thtest/app/Main.hs:79:3
--
-- >>> _3 (42, "hello", '#', [], 3.14)
-- '#'
--
-- >>> _10 (42, "hello", '#', [], 3.14, 42, "hello", '#', [], 3.14)
-- 3.14

-------------
-- Summary --
-------------

-- Compilation process consists of two steps:
-- 1st step: Parsing (source code = text) → AST
-- 2nd step: Compiling AST → runnable code
--
--         1st step
--             |   2nd step
--             |     |
-- source code → AST → runnable code
--
-- [| ... |] quoter = 1st step

-- $ (splicing)     = 2nd step

main ∷ IO ()
main = putStrLn "Hello, Haskell!"
