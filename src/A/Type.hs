-- | conversion of 'Language.Haskell.Exts.Simple.Syntax.Type' to
-- 'Language.Haskell.Syntax.Type'
module A.Type (ToTH (..), cvtnam, cvtder) where

import A.ISyn (Derive (..))
import Data.List (foldl')
import Data.Maybe (fromMaybe)
import Language.Haskell.Exts.Simple.Pretty as Exts
import Language.Haskell.Exts.Simple.Syntax as Exts
import Language.Haskell.TH as TH

-- | convert a 'Type' to a 'TH.Type'
class ToTH a where
  -- | convert a 'Type' to a 'TH.Type'
  toth :: a -> TH.Type

instance ToTH Exts.Type where
  toth = \case
    -- basic mappings
    TyVar n -> VarT (mkName $ prettyPrint n)
    TyCon n -> ConT (mkName $ prettyPrint n)
    TyApp f x -> AppT (toth f) (toth x)
    TyList t -> AppT ListT (toth t)
    TyParen t -> ParensT (toth t)
    -- tuples need special handling
    TyTuple _ ts ->
      let n = length ts
          apps = map toth ts
       in foldl' AppT (TupleT n) apps
    -- functions
    TyFun a b -> AppT (AppT ArrowT (toth a)) (toth b)
    -- complex cases
    TyForall tvs ctx t ->
      let tvs' = maybe [] (map tothtyvarbndr) tvs
          ctx_ = case ctx of
            Just (CxSingle a) -> [a]
            Just (CxTuple as) -> as
            Just CxEmpty -> []
            _ -> []
          ctx' = map tothpred ctx_
       in ForallT tvs' ctx' (toth t)
    TyPromoted prom -> case prom of
      PromotedInteger n _ ->
        LitT (NumTyLit n)
      PromotedString s _ ->
        LitT (StrTyLit s)
      PromotedCon _ n ->
        PromotedT (mkName $ prettyPrint n)
      PromotedList _ ts ->
        foldr
          ((\x acc -> PromotedConsT `AppT` x `AppT` acc) . toth)
          PromotedNilT
          ts
      PromotedTuple ts ->
        foldl' AppT (PromotedTupleT (length ts)) (map toth ts)
      PromotedUnit ->
        PromotedTupleT 0
    -- other cases need implementation
    TyStar -> error "toth: unsupported TyStar"
    TyUnboxedSum {} -> error "toth: unsupported TyUnboxedSum"
    TyParArray {} -> error "toth: unsupported TyParArray"
    TyInfix {} -> error "toth: unsupported TyInfix"
    TyKind {} -> error "toth: unsupported TyKind"
    TyEquals {} -> error "toth: unsupported TyEquals"
    TySplice {} -> error "toth: unsupported TySplice"
    TyBang {} -> error "toth: unsupported TyBang"
    TyWildCard {} -> error "toth: unsupported TyWildCard"
    TyQuasiQuote {} -> error "toth: unsupported TyQuasiQuote"

tothtyvarbndr :: TyVarBind -> TyVarBndr Specificity
tothtyvarbndr = \case
  KindedVar n k ->
    let k' = toth k
     in KindedTV (mkName $ prettyPrint n) SpecifiedSpec k'
  UnkindedVar n ->
    PlainTV (mkName $ prettyPrint n) SpecifiedSpec

tothpred :: Asst -> TH.Type
tothpred = \case
  TypeA n -> toth n
  IParam n t ->
    let t' = toth t
     in ImplicitParamT (drop 1 $ prettyPrint n) t' -- drop '?'
  ParenA a -> tothpred a

-- | convert a Exts.'Exts.Name' to a TH.'TH.Name'
--
-- qualification gets handled syntactically
cvtnam :: Exts.Name -> TH.Name
cvtnam = mkName . prettyPrint

-- | convert a 'Derive' to a list of TH.'TH.Type's
cvtder :: Derive -> [TH.Type]
cvtder = map (ConT . mkName) . getderive

-- | used for testing
--
-- >>> :t $(_dbgasexp (pure $ toth mytype))
_dbgasexp :: TH.Type -> Q TH.Exp
_dbgasexp t = pure $ SigE (VarE 'undefined) t
