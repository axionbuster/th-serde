-- | template Haskell generator
module Data.Serde.Internal.TH
  ( runqq1,
    runusercoercion,
    RunUserCoercion (..),
  )
where

import Data.Coerce
import Data.Data
import Data.Foldable
import Data.List (partition)
import Data.Maybe
import Data.Serde.Internal.Syn
import Data.Serde.Internal.Type
import Data.Traversable
import Language.Haskell.TH.Lib as TH
import Language.Haskell.TH.Syntax as TH

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

-- generate regular data
gendat :: Syn -> Dec
gendat SynData {synnam, synflds, synders} =
  let n = cvtnam synnam
      flds = map genfld synflds
      der = DerivClause Nothing [ConT $ cvtnam c | c <- synders]
   in DataD [] n [] Nothing [RecC n flds] [der]
gendat _ = error "gendat: not a data type"

-- generate shadow data
genshadata :: Syn -> Dec
genshadata SynData {synnam, synflds} =
  let n = shadownam $ cvtnam synnam
      flds = map genshafld synflds
   in DataD [] n [] Nothing [RecC n flds] [] -- derive nothing
genshadata _ = error "genshadata: not a data type"

-- generate newtype
gennew :: Syn -> Dec
gennew SynNewtype {synnam, synfld, synders} =
  let n = cvtnam synnam
      der Nothing = DerivClause Nothing [ConT $ cvtnam c | c <- synders]
      der (Just t) =
        DerivClause
          (Just (ViaStrategy t))
          [ConT $ cvtnam c | c <- synders]
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

-- has a shadowing field?
shadowing :: Syn -> Bool
shadowing SynData {synflds} = any (isJust . synfvia) synflds
shadowing _ = False

-- generate declarations for a 'Syn' object (e.g., data, newtype, alias)
gendecs :: Syn -> [Dec]
gendecs s@SynData {} = gendat s : sha
  where
    sha
      | shadowing s = [genshadata s]
      | otherwise = []
gendecs s@SynNewtype {} = [gennew s]
gendecs s@SynAlias {} = [genalias s]

-- generate a pattern to deconstruct a normal constructor
genfuncctor :: Syn -> Q Pat
genfuncctor SynData {synnam, synflds} =
  let n = cvtnam synnam
   in conP n (map (varP . cvtnam . synfnam) synflds)
genfuncctor _ = fail "genfuncctor: not a data type"

-- generate a pattern to deconstruct a shadow constructor
--
-- fields will have a shadow suffix
genfuncctorsha :: Syn -> Q Pat
genfuncctorsha SynData {synnam, synflds} =
  let n = shadownam $ cvtnam synnam
   in conP n (map (varP . shadownam . cvtnam . synfnam) synflds)
genfuncctorsha _ = fail "genfuncctorsha: not a data type"

-- apply shadow fields to a normal constructor
genapp :: Syn -> Q Exp
genapp SynData {synnam, synflds} =
  let n = cvtnam synnam
      v = (map (varE . shadownam . cvtnam . synfnam) synflds)
   in foldl' appE (conE n) (appE (varE 'coerce) <$> v)
genapp _ = fail "genapp: not a data type"

-- apply normal fields to a shadow constructor
genappsha :: Syn -> Q Exp
genappsha SynData {synnam, synflds} =
  let n = shadownam $ cvtnam synnam
      v = (map (varE . cvtnam . synfnam) synflds)
   in foldl' appE (conE n) (appE (varE 'coerce) <$> v)
genappsha _ = fail "genappsha: not a data type"

-- to prevent getQ from silently failing (by fixing the type)
-- remember: getQ relies on Typeable
newtype QQState = QQState {qqstate :: [Syn]} -- declarations
  deriving (Typeable)

-- | run quasi-quote body, and replace Q state (to get the
-- shadowable data types)
runqq1 :: String -> Q [Dec]
runqq1 s = case parse s of
  Left e -> fail e
  Right p -> do
    putQ (QQState (declarations p))
    pure (concat [gendecs t | t <- declarations p])

-- | arguments to user code that generates coercions
data RunUserCoercion = RunUserCoercion
  { -- | regular (non-record syntax) pattern for deconstruction (normal)
    patnormal :: Q Pat,
    -- | regular (non-record syntax) pattern for deconstruction (shadow)
    patshadow :: Q Pat,
    -- | apply shadow fields to a normal constructor
    appnormal :: Q Exp,
    -- | apply normal fields to a shadow constructor
    appshadow :: Q Exp,
    -- | class to derive
    datatyp :: Q TH.Type,
    -- | shadow data type
    shadowdatatyp :: Q TH.Type
  }

-- | using the stored state (from last quasi-quote run), run user code
-- to generate coercions
runusercoercion ::
  -- | generate coercions between shadow and regular data types
  (RunUserCoercion -> Q [Dec]) ->
  -- | derive coercions for shadow data, regular data with no shadows,
  -- and newtypes
  (TH.Name -> Q [Dec]) ->
  -- | preparations for shadow types
  [TH.Name] ->
  -- | generated coercions
  Q [Dec]
runusercoercion f g (fmap ConT -> preps) = do
  let (++++) = liftA2 (++)
  -- get all the shadowable data types and the regular data types
  let isnotalias SynAlias {} = False
      isnotalias _ = True
  (ss, ns) <-
    getQ >>= \case
      Just t -> pure $ partition shadowing $ filter isnotalias $ qqstate t
      Nothing -> fail "runusercoercion: run serde quasi-quote first"
  -- standaloneDerivD is used to generate standalone deriving instances
  -- for the shadow types
  let toderive =
        concat
          [ [ (conT . shadownam . cvtnam . synnam $ s, c),
              (conT . cvtnam . synnam $ s, c)
            ]
          | s <- ss,
            c <- preps
          ]
          ++ [(conT . cvtnam . synnam $ n, c) | n <- ns, c <- preps]
      derives = for toderive \(s, c) ->
        standaloneDerivD (pure []) (appT (pure c) s)
  derives
    ++++ mconcat [g (shadownam . cvtnam . synnam $ s) | s <- ss]
    ++++ mconcat [g (cvtnam . synnam $ n) | n <- ns]
    ++++ mconcat
      [ f
          RunUserCoercion
            { patnormal = genfuncctor s,
              patshadow = genfuncctorsha s,
              appnormal = genapp s,
              appshadow = genappsha s,
              datatyp = conT . cvtnam . synnam $ s,
              shadowdatatyp = conT . shadownam . cvtnam . synnam $ s
            }
      | s <- ss
      ]
