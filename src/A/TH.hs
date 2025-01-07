-- | template Haskell generator
module A.TH (R, gendecs) where

import A.Syn
import A.Type
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Language.Haskell.TH as TH
import Language.Haskell.TH.Quote as TH
import Language.Haskell.TH.Syntax as TH hiding (lift)

-- internal reader monad for the generator
type R = ReaderT Parsed Q

bang0 :: Bang
bang0 = Bang NoSourceUnpackedness NoSourceStrictness

-- regular field type (ignore via)
genfld :: SynFld -> VarBangType
genfld SynFld {synfnam, synftyp} =
  let n = cvtnam synfnam
      t = toth synftyp
   in (n, bang0, t)

-- suffix with two underscores
shadownamcore :: String -> String
shadownamcore = (++ "__")

shadownam :: TH.Name -> TH.Name
shadownam (TH.Name (OccName o) s) = TH.Name (OccName (shadownamcore o)) s

-- shadow field type (ignore type, use via)
genshafld :: SynFld -> VarBangType
genshafld SynFld {synfnam, synftyp, synfvia} =
  let n = shadownam $ cvtnam synfnam
      t
        | Just v <- synfvia = toth v
        | otherwise = toth synftyp
   in (n, bang0, t)

-- | generate regular data
gendat :: Syn -> Dec
gendat SynData {synnam, synflds, synders} =
  let n = cvtnam synnam
      flds = map genfld synflds
      der = DerivClause Nothing [ConT $ cvtnam c | c <- synders]
   in DataD [] n [] Nothing [RecC n flds] [der]
gendat _ = error "gendat: not a data type"

-- generate shadow data
genshadata :: Syn -> Dec
genshadata SynData {synnam, synflds, synders} =
  let n = shadownam $ cvtnam synnam
      flds = map genshafld synflds
      der = DerivClause Nothing [ConT $ cvtnam c | c <- synders]
   in DataD [] n [] Nothing [RecC n flds] [der]
genshadata _ = error "genshadata: not a data type"

-- generate newtype
gennew :: Syn -> Dec
gennew SynNewtype {synnam, synfld, synders} =
  let n = cvtnam synnam
      der Nothing = DerivClause Nothing [ConT $ cvtnam c | c <- synders]
      der (Just t) = DerivClause (Just (ViaStrategy t)) [ConT $ cvtnam c | c <- synders]
      mk a v = NewtypeD [] n [] Nothing a [der v]
   in case synfld of
        Left (Plain t) -> mk (NormalC n [(bang0, ConT (mkName t))]) Nothing
        Left (WithVia t v) ->
          mk (NormalC n [(bang0, ConT (mkName t))]) (Just (ConT (mkName v)))
        Right f@SynFld {synfvia} -> mk (RecC n [genfld f]) (toth <$> synfvia)
gennew _ = error "gennew: not a newtype"

-- generate an alias
genalias :: Syn -> Dec
genalias SynAlias {synnam, syndest} =
  let n = cvtnam synnam
      t = toth syndest
   in TySynD n [] t
genalias _ = error "genalias: not an alias"

-- | generate a declaration
gendecs :: Syn -> [Dec]
gendecs s@SynData {} = [gendat s, genshadata s]
gendecs s@SynNewtype {} = [gennew s]
gendecs s@SynAlias {} = [genalias s]
