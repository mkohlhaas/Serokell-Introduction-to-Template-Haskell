{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

module TuplesTH (generateTupleClass, generateTupleInstance, generateTupleBoilerplate) where

import Control.Monad (unless)
import Data.Traversable (for)
import Data.List (foldl')
import Language.Haskell.TH

-------------
-- Classes --
-------------

-- class Tuple1 t r | t → r where
--   _1 ∷ t → r
--
-- class Tuple2 t r | t → r where
--   _2 ∷ t → r
--
-- ...

-- >>> runQ [d| class Tuple1 t r | t → r where _1 ∷ t → r |]
-- [ClassD [] Tuple1 [PlainTV t,PlainTV r] [FunDep [t] [r]] [SigD _1 (AppT (AppT ArrowT (VarT t)) (VarT r))]]
--
-- >>> runQ [d| class Tuple2 t r | t → r where _2 ∷ t → r |]
-- [ClassD [] Tuple2 [PlainTV t,PlainTV r] [FunDep [t] [r]] [SigD _2 (AppT (AppT ArrowT (VarT t)) (VarT r))]]
--
-- ...

generateTupleClass ∷ Int → Q [Dec]
generateTupleClass size = do
  unless (size > 0) (fail $ "Non-positive size: " <> size')
  pure [cDecl]
  where
    size' ∷ String
    size' = show size
    className ∷ Name
    className = mkName ("Tuple" <> size')
    methodName ∷ Name
    methodName = mkName ('_' : size')
    t ∷ Name
    t = mkName "t"
    r ∷ Name
    r = mkName "r"
    cDecl ∷ Dec
    cDecl = ClassD [] className [PlainTV t, PlainTV r] [FunDep [t] [r]] [mDecl]
    -- In GHC 9: cDecl = ClassD [] className [PlainTV t (), PlainTV r ()] [FunDep [t] [r]] [mDecl]
    mDecl ∷ Dec
    mDecl = SigD methodName (AppT (AppT ArrowT (VarT t)) (VarT r))

---------------
-- Instances --
---------------

-- test classes
class Tuple1 t r | t → r where
  _1 ∷ t → r

class Tuple2 t r | t → r where
  _2 ∷ t → r

class Tuple3 t r | t → r where
  _3 ∷ t → r

-- instance Tuple1 (t1, t2) t1 where
--   _1 (x, _) = x
--
-- instance Tuple2 (t1, t2) t2 where
--   _2 (_, x) = x
--
-- ...

-- element 1, size 2
-- >>> runQ [d| instance Tuple1 (t1, t2) t1 where _1 (x, _) = x |]
-- [InstanceD Nothing [] (AppT (AppT (ConT TuplesTH.Tuple1) (AppT (AppT (TupleT 2) (VarT t1)) (VarT t2))) (VarT t1)) [FunD TuplesTH._1 [Clause [TupP [VarP x,WildP]] (NormalB (VarE x)) []]]]
--
-- element 2, size 2
-- >>> runQ [d| instance Tuple2 (t1, t2) t2 where _2 (_, x) = x |]
-- [InstanceD Nothing [] (AppT (AppT (ConT TuplesTH.Tuple2) (AppT (AppT (TupleT 2) (VarT t1)) (VarT t2))) (VarT t2)) [FunD TuplesTH._2 [Clause [TupP [WildP,VarP x]] (NormalB (VarE x)) []]]]

-- element 1, size 3
-- >>> runQ [d| instance Tuple1 (t1, t2, t3) t1 where _1 (x, _, _) = x |]
-- [InstanceD Nothing [] (AppT (AppT (ConT TuplesTH.Tuple1) (AppT (AppT (AppT (TupleT 3) (VarT t1)) (VarT t2)) (VarT t3))) (VarT t1)) [FunD TuplesTH._1 [Clause [TupP [VarP x,WildP,WildP]] (NormalB (VarE x)) []]]]
--
-- element 2, size 3
-- >>> runQ [d| instance Tuple2 (t1, t2, t3) t2 where _2 (_, x, _) = x |]
-- [InstanceD Nothing [] (AppT (AppT (ConT TuplesTH.Tuple2) (AppT (AppT (AppT (TupleT 3) (VarT t1)) (VarT t2)) (VarT t3))) (VarT t2)) [FunD TuplesTH._2 [Clause [TupP [WildP,VarP x,WildP]] (NormalB (VarE x)) []]]]
--
-- element 3, size 3
-- >>> runQ [d| instance Tuple3 (t1, t2, t3) t3 where _3 (_, _, x) = x |]
-- [InstanceD Nothing [] (AppT (AppT (ConT TuplesTH.Tuple3) (AppT (AppT (AppT (TupleT 3) (VarT t1)) (VarT t2)) (VarT t3))) (VarT t3)) [FunD TuplesTH._3 [Clause [TupP [WildP,WildP,VarP x]] (NormalB (VarE x)) []]]]

generateTupleInstance ∷ Int → Int → Q [Dec]
generateTupleInstance element size = do
  unless (size > 0) (fail $ "Non-positive size: " <> element')
  unless (size >= element) (fail $ "Can't extract element " <> element' <> " of " <> show size <> "-tuple")
  pure [iDecl]
  where
    vars ∷ [Name]
    vars = [mkName ('t' : show n) | n ← [1 .. size]]
    signature ∷ Type
    signature = foldl' (\acc var → AppT acc (VarT var)) (TupleT size) vars
    iDecl ∷ Dec
    element' ∷ String
    element' = show element
    className ∷ Name
    className = mkName ("Tuple" <> element')
    iDecl = InstanceD Nothing [] (AppT (AppT (ConT className) signature) (VarT $ mkName ('t' : element'))) [mDecl]
    mDecl ∷ Dec
    methodName ∷ Name
    methodName = mkName ('_' : element')
    x ∷ Name
    x = mkName "x"
    mDecl = FunD methodName [Clause [TupP $ replicate (element - 1) WildP <> [VarP x] <> replicate (size - element) WildP] (NormalB $ VarE x) []]

-- TODO: might be better with list comprehensions
generateTupleBoilerplate ∷ Int → Q [Dec]
generateTupleBoilerplate size =
  concatFor [1 .. size] $ \classDeclIndex → do
    cDecl ← generateTupleClass classDeclIndex
    iDecls ← for [1 .. classDeclIndex] $ \instanceDeclIndex →
      generateTupleInstance instanceDeclIndex classDeclIndex
    pure $ concat (cDecl : iDecls)
  where
    concatFor ∷ [Int] → (Int → Q [Dec]) → Q [Dec]
    concatFor xs c = concat <$> for xs c
