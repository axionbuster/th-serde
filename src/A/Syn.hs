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
-- ___ @.coerce@ ___
--
-- @.coerce@ defines how to coerce between the shadow type and the data type
--
-- type class name followed by a function that converts the shadow type and
-- the data type to a list of declarations
--
-- both the type class and the function must be in scope
--
-- ___ @.derive@ ___
--
-- @.derive@ defines which type classes to derive for all types defined in
-- the module
--
-- they are not given in the shadow type, but in the data type
--
-- shadow types only implement: 'Generic', 'Typeable', and 'Data'
module A.Syn where

import Control.Applicative.Combinators
import Control.Monad
import Data.Maybe
import Data.Void
import Language.Haskell.Exts.Extension
import Language.Haskell.Exts.Parser
import Language.Haskell.Exts.SrcLoc
import Language.Haskell.Exts.Syntax
import Text.Megaparsec (Parsec)
import Text.Megaparsec qualified as M
import Text.Megaparsec.Char qualified as M
import Text.Megaparsec.Char.Lexer qualified as L
import Text.Megaparsec.Debug (dbg)

type Parser = Parsec Void String

data Syn
  = SynData {synnam :: String, synflds :: [SynFld]}
  | SynNewtype {synnam :: String, synfld1 :: SynFld}
  | SynAlias {synnam :: String, syndest :: String}
  deriving (Show)

data SynFld
  = SynFld {synfldnam :: String, synfldtyp :: String, synfldvia :: String}
  deriving (Show)

pm1 =
  defaultParseMode
    { extensions =
        [ EnableExtension DataKinds,
          EnableExtension TypeApplications
        ]
    }

data ISyn
  = ISynData {isnam :: String, isflds :: [ISynFld]}
  | ISynNewtype {isnam :: String, isfld1 :: INewtypePayload}
  | ISynAlias {isnam :: String, isdest :: String}
  deriving (Show)

data INewtypePayload
  = IField ISynFld
  | IType ViaInfo
  deriving (Show)

data ISynFld
  = ISynFld {isfldnam :: String, isfldtyp :: ViaInfo}
  deriving (Show)

data CoercePair = CoercePair
  { cpclass :: String,
    cpfun :: String
  }
  deriving (Show)

newtype CoerceHeader = CoerceHeader {getcoerceheader :: [CoercePair]}
  deriving (Show)

data Derive' = Derive' {getderive' :: [String]}
  deriving (Show)

sc :: Parser ()
sc = L.space
  do M.space1
  do L.skipLineComment "--"
  do L.skipBlockComment "{-" "-}"

-- roughly an identifier; a non-strict superset
identifier :: Parser String
identifier = (:) <$> headchar <*> M.many bodychar
  where
    headchar = M.letterChar <|> M.char '_' <|> M.char '\''
    bodychar = M.alphaNumChar <|> M.char '_' <|> M.char '\''

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

scnonl :: Parser ()
scnonl = L.space
  do void $ some (M.char ' ' <|> M.char '\t') -- no newline
  do L.skipLineComment "--"
  do L.skipBlockComment "{-" "-"

data ViaInfo
  = Plain String
  | WithVia String String
  deriving (Show)

-- always succeeds
untileol :: Parser String
untileol = M.manyTill M.anySingle do
  M.lookAhead (M.eof <|> void M.eol)

untileol' :: Parser Char -> Parser String
untileol' p = M.manyTill p do
  M.lookAhead (M.eof <|> void M.eol)

-- fails if there is no via
untilvia :: Parser String
untilvia = M.manyTill M.anySingle do
  M.lookAhead (M.try (void (M.space1 *> M.string "via")))

itype :: Parser ViaInfo
itype =
  choice
    [ WithVia
        <$> M.try (lexeme untilvia)
        <*> M.try (lexeme (M.string "via") *> lexeme untileol),
      Plain <$> untileol
    ]

iparsefield :: Parser ISynFld
iparsefield = do
  -- fieldname :: type via via
  isfldnam <- lexeme identifier
  void $ lexeme (M.string "::")
  isfldtyp <- itype
  pure $ ISynFld {isfldnam, isfldtyp}

-- parse an indented block
block :: Parser a -> Parser [a]
block p = many do L.indentGuard sc GT M.pos1 *> p

iparsedata :: Parser ISyn
iparsedata = do
  void $ lexeme (M.string "data")
  isnam <- lexeme identifier
  isflds <- block do iparsefield
  pure $ ISynData {isnam, isflds}

-- using record syntax
iparsenewtype1 :: Parser ISyn
iparsenewtype1 = do
  void $ lexeme (M.string "newtype")
  isnam <- lexeme identifier
  fields <- block do iparsefield
  case fields of
    [IField -> isfld1] -> pure $ ISynNewtype {isnam, isfld1}
    _ -> fail "newtype must have exactly one field"

-- using non-record syntax
iparsenewtype2 :: Parser ISyn
iparsenewtype2 = do
  void $ lexeme (M.string "newtype")
  isnam <- lexeme identifier
  isfld1 <- IType <$> itype
  pure $ ISynNewtype {isnam, isfld1}

iparsenewtype :: Parser ISyn
iparsenewtype = M.try iparsenewtype1 <|> iparsenewtype2

itypealias :: Parser ISyn
itypealias = do
  void $ lexeme (M.string "type")
  isnam <- lexeme identifier
  isdest <- lexeme untileol
  pure $ ISynAlias {isnam, isdest}

coerceheader :: Parser CoerceHeader
coerceheader = do
  -- directives after .coerce must begin in the next line
  void $ lexeme (M.string ".coerce" <* M.hspace <* M.eol)
  CoerceHeader . catMaybes <$> block do
    -- the very last word is the function name
    -- everything to the left is the class name
    -- NOTE ON LAZINESS: diverges if not matched strictly
    !names <- words <$> M.someTill M.anySingle (void M.eol <|> M.eof)
    case names of
      [] -> pure Nothing
      [_] -> fail "no class name"
      _ ->
        pure $
          Just $
            CoercePair
              { cpclass = unwords $ init names,
                cpfun = last names
              }

deriveheader :: Parser Derive'
deriveheader = do
  -- list doesn't have to begin in the next line
  void $ lexeme (M.string ".derive" <* M.space1)
  -- see note on laziness above
  !a <-
    Derive' . words . unwords <$> block do
      M.someTill M.anySingle (void M.eol <|> M.eof)
  pure a

header :: Parser (CoerceHeader, Derive')
header = (,) <$> coerceheader <*> deriveheader
