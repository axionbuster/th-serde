-- | conversion of 'Language.Haskell.Exts.Simple.Syntax.Type' to
-- 'Language.Haskell.Syntax.Type'
module A.Type (ToTH (..)) where

import Data.List (foldl')
import Language.Haskell.Exts.Simple.Pretty as Exts
import Language.Haskell.Exts.Simple.Syntax as Exts
import Language.Haskell.TH as TH

-- | convert a 'Type' to a 'TH.Type'
class ToTH a where
  -- | convert a 'Type' to a 'TH.Type'
  toth :: a -> Q TH.Type

instance ToTH Exts.Type where
  toth = \case
    -- basic mappings
    TyVar n -> pure $ VarT (mkName $ prettyPrint n)
    TyCon n -> pure $ ConT (mkName $ prettyPrint n)
    TyApp f x -> AppT <$> toth f <*> toth x
    TyList t -> pure ListT `appT` toth t
    TyParen t -> ParensT <$> toth t
    -- tuples need special handling
    TyTuple _ ts -> do
      let n = length ts
      apps <- traverse toth ts
      pure $ foldl' AppT (TupleT n) apps
    -- functions
    TyFun a b -> pure ArrowT `appT` toth a `appT` toth b
    -- complex cases
    TyForall tvs ctx t -> do
      tvs' <- traverse tothtyvarbndr (maybe [] id tvs)
      let ctx_
            | Just (CxSingle a) <- ctx = [a]
            | Just (CxTuple as) <- ctx = as
            | Just CxEmpty <- ctx = []
            | otherwise = []
      ctx' <- traverse tothpred ctx_
      t' <- toth t
      pure $ ForallT tvs' ctx' t'
    TyPromoted prom -> case prom of
      PromotedInteger n _ ->
        pure $ LitT (NumTyLit n)
      PromotedString s _ ->
        pure $ LitT (StrTyLit s)
      PromotedCon _ n ->
        pure $ PromotedT (mkName $ prettyPrint n)
      PromotedList _ ts -> do
        ts' <- traverse toth ts
        pure $ foldr (\x acc -> PromotedConsT `AppT` x `AppT` acc) PromotedNilT ts'
      PromotedTuple ts -> do
        ts' <- traverse toth ts
        pure $ foldl' AppT (PromotedTupleT (length ts)) ts'
      PromotedUnit ->
        pure $ PromotedTupleT 0
    -- other cases need implementation
    TyStar -> fail "toth: unsupported TyStar"
    TyUnboxedSum _ -> fail "toth: unsupported TyUnboxedSum"
    TyParArray _ -> fail "toth: unsupported TyParArray"
    TyInfix _ _ _ -> fail "toth: unsupported TyInfix"
    TyKind _ _ -> fail "toth: unsupported TyKind"
    TyEquals _ _ -> fail "toth: unsupported TyEquals"
    TySplice _ -> fail "toth: unsupported TySplice"
    TyBang _ _ _ -> fail "toth: unsupported TyBang"
    TyWildCard _ -> fail "toth: unsupported TyWildCard"
    TyQuasiQuote _ _ -> fail "toth: unsupported TyQuasiQuote"

tothtyvarbndr :: TyVarBind -> Q (TyVarBndr Specificity)
tothtyvarbndr = \case
  KindedVar n k -> do
    k' <- toth k
    pure $ KindedTV (mkName $ prettyPrint n) SpecifiedSpec k'
  UnkindedVar n ->
    pure $ PlainTV (mkName $ prettyPrint n) SpecifiedSpec

tothpred :: Asst -> Q TH.Pred
tothpred = \case
  TypeA n -> toth n
  IParam n t -> do
    t' <- toth t
    -- drop the '?' prefix
    pure $ ImplicitParamT (drop 1 $ prettyPrint n) t'
  ParenA a -> tothpred a
