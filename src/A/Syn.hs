{-# OPTIONS_GHC -Wno-partial-fields #-}

-- | define syntax
--
-- lightweight syntax for defining data types, newtypes, and type aliases
-- for serialization and validation
--
-- __ synopsys __
--
-- we want to separate how to serialize and validate data from the data itself
--
-- that's usually done by defining newtypes and coercing between them
--
-- but that only works for newtypes, not for data types
--
-- data types with identical or coercible fields cannot be coerced,
-- which is a problem when deriving certain type classes through
-- newtype coercion
--
-- this module provides a lightweight syntax for defining data types and
-- newtypes
--
-- here a data type gets split in two: the shadow type and the data type
--
-- the shadow type has fields that are coercible to the data type's fields
--
-- the shadow type implements the serialization and validation logic
-- and the data type is constructed/deconstructed by coercing to/from
-- the shadow type
--
-- ultimately, the user is responsible for going the last mile, but
-- this module provides a lot of the boilerplate
--
-- __ example __
--
-- @
-- data Person
--  age :: Int32 via Age
--  name :: String via (VerifyLength 1 10 String)
--  email :: String via (VerifyEmail String)
--
-- newtype Age
--  getage :: Int32
--
-- newtype Great String
--
-- type MyAlias String
-- @
--
-- in the example above, @Person@ gets defined alongside its shadow type
-- @Person__@, which has coercible fields to @Person@
--
-- @
-- data Person__ = Person__
--  { age__ :: Age,
--    name__ :: VerifyLength 1 10 String,
--    email__ :: VerifyEmail String
--  }
-- @
--
-- it also defines the main type, @Person@:
--
-- @
-- data Person = Person
--  { age :: Int32,
--    name :: String,
--    email :: String
--  }
-- @
--
-- as one can see, the newtypes that define _how_ to serialize and validate
-- the data are separated from the data itself
--
-- __ header __
--
-- there is a header part that's needed to define the shadow type coercions
--
-- @
-- .coerce
--  Pack mkpackdecls
--  Unpack mkunpackdecls
--
-- .derive
--  Eq Ord Show Read
--  Generic Typeable Data
-- @
--
-- __ @.coerce@ __
--
-- @.coerce@ defines how to coerce between the shadow type and the data type
--
-- type class name followed by a function that converts the shadow type and
-- the data type to a list of declarations
--
-- both the type class and the function must be in scope
--
-- __ @.derive@ __
--
-- @.derive@ defines which type classes to derive for all types defined in
-- the module
--
-- they are not given in the shadow type, but in the data type
--
-- shadow types only implement: 'Generic', 'Typeable', and 'Data'
module A.Syn
  ( -- * parsing
    Parsed (..),
    parse,

    -- * syntax
    Syn (..),
    SynFld (..),
    ViaInfo (..),
    CoerceHeader (..),
    CoercePair (..),

    -- * re-export
    Name,
    Type,
  )
where

import A.ISyn
import Language.Haskell.Exts.Simple.Extension
import Language.Haskell.Exts.Simple.Parser hiding (parse)
import Language.Haskell.Exts.Simple.Syntax
import Text.Megaparsec qualified as M

-- | parsed data
data Parsed = Parsed
  { declarations :: [Syn],
    coersions :: CoerceHeader,
    derives :: Derive
  }
  deriving (Show)

-- | a declaration
data Syn
  = SynData
      { synnam :: Name, -- type/con name
        synflds :: [SynFld], -- fields
        synders :: [Name] -- deriving classes
      }
  | SynNewtype
      { synnam :: Name,
        synfld :: Either ViaInfo SynFld,
        synders :: [Name]
      }
  | SynAlias
      { synnam :: Name,
        syndest :: Type -- destination type
      }
  deriving (Show)

-- | field information
data SynFld = SynFld
  { synfnam :: Name, -- field name
    synftyp :: Type, -- target type
    synfvia :: Maybe Type -- via type if any
  }
  deriving (Show)

pm1 :: ParseMode
pm1 =
  defaultParseMode
    { extensions =
        [ EnableExtension DataKinds,
          EnableExtension TypeApplications
        ]
    }

-- | convert intermediate syntax to syntax
fromisyn :: Derive -> ISyn -> Syn
fromisyn der0 = do
  let der = fromderive der0
  \case
    ISynData n fs -> SynData (name n) (map fromisynfld fs) der
    ISynNewtype n (IField f) ->
      SynNewtype (name n) (Right (fromisynfld f)) der
    ISynNewtype n (IType v) -> case fromviainfo v of
      Left _ -> SynNewtype (name n) (Left v) der
      Right (s, u) ->
        SynNewtype (name n) (Right (SynFld (name n) s (Just u))) der
    ISynAlias n d -> SynAlias (name n) (parsetype d)
  where
    name = Ident
    parsetype t = case parseTypeWithMode pm1 t of
      ParseOk x -> x
      ParseFailed _ e -> error $ "parsetype: " ++ show e
    fromderive = fmap name . getderive
    fromisynfld (ISynFld n t) = case fromviainfo t of
      Left v -> SynFld (name n) v Nothing
      Right (s, u) -> SynFld (name n) s (Just u)
    fromviainfo (Plain t) = Left (parsetype t)
    fromviainfo (WithVia t s) = Right (parsetype t, parsetype s)

-- | parse quasi-quoted syntax
parse :: String -> Either String Parsed
parse s =
  let res = M.runParser (parsetop <* M.eof) "" s
   in case res of
        Right ((co, de), fmap (fromisyn de) -> ds) ->
          Right $ Parsed ds co de
        Left e -> Left $ M.errorBundlePretty e
