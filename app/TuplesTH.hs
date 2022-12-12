{-# LANGUAGE TemplateHaskell #-}

module TuplesTH where

import Control.Monad (unless)
import Data.Traversable (for)
import Language.Haskell.TH

-- >>> runQ [d|x ∷ (a,b,c); x = undefined|]
-- [SigD x_19 (AppT (AppT (AppT (TupleT 3) (VarT a_16)) (VarT b_17)) (VarT c_18)),ValD (VarP x_19) (NormalB (VarE GHC.Err.undefined)) []]
--
--
-- >>> runQ [d|x ∷ a → b; x = undefined|]
-- [SigD x_22 (AppT (AppT ArrowT (VarT a_20)) (VarT b_21)),ValD (VarP x_22) (NormalB (VarE GHC.Err.undefined)) []]

generateTupleClass ∷ Int → Q [Dec]
generateTupleClass size = do
  unless (size > 0) $
    fail $ "Non-positive size: " ++ size'
  pure [cDecl]
  where
    size' ∷ String
    size' = show size
    className ∷ Name
    className = mkName ("Tuple" ++ size')
    methodName ∷ Name
    methodName = mkName ('_' : size')
    t ∷ Name
    t = mkName "t"
    r ∷ Name
    r = mkName "r"
    -- class TupleX t r | t → r where
    -- In GHC 9: cDecl = ClassD [] className [PlainTV t (), PlainTV r ()] [FunDep [t] [r]] [mDecl]
    cDecl ∷ Dec
    cDecl = ClassD [] className [PlainTV t, PlainTV r] [FunDep [t] [r]] [mDecl]
    --   _X ∷ t → r
    mDecl ∷ Dec
    mDecl = SigD methodName (AppT (AppT ArrowT (VarT t)) (VarT r))

generateTupleInstance ∷ Int → Int → Q [Dec]
generateTupleInstance element size = do
  unless (size > 0) $
    fail $ "Non-positive size: " ++ element'
  unless (size >= element) $
    fail $ "Can't extract element " ++ element' ++ " of " ++ size' ++ "-tuple"
  pure [iDecl]
  where
    element' ∷ String
    element' = show element
    size' ∷ String
    size' = show size
    className ∷ Name
    className = mkName ("Tuple" ++ element')
    methodName ∷ Name
    methodName = mkName ('_' : element')
    x ∷ Name
    x = mkName "x"
    vars ∷ [Name]
    vars = [mkName ('t' : show n) | n ← [1 .. size]]
    signature ∷ Type
    signature = foldl (\acc var → AppT acc (VarT var)) (TupleT size) vars
    -- instance TupleX (t1, ..., tX, ...) tX where
    iDecl ∷ Dec
    iDecl = InstanceD Nothing [] (AppT (AppT (ConT className) signature) (VarT $ mkName ('t' : element'))) [mDecl]
    --   _X (_, _, ..., x, ...) = x
    mDecl ∷ Dec
    mDecl = FunD methodName [Clause [TupP $ replicate (element - 1) WildP ++ [VarP x] ++ replicate (size - element) WildP] (NormalB $ VarE x) []]

generateTupleBoilerplate ∷ Int → Q [Dec]
generateTupleBoilerplate size =
  concatFor [1 .. size] $ \classDeclIndex → do
    cDecl ← generateTupleClass classDeclIndex
    iDecls ← for [1 .. classDeclIndex] $ \instanceDeclIndex ->
      generateTupleInstance instanceDeclIndex classDeclIndex

    pure $ concat (cDecl : iDecls)
  where
    -- concatFor ∷ t a → (a → f [a1]) → f [a1]
    concatFor xs = fmap concat . for xs
