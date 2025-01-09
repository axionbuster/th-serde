{-# OPTIONS_GHC -Wno-partial-fields #-}

-- | intermediate syntax for the parser
--
-- see "Data.Serde.Syn" for the grammar
module Data.Serde.Internal.ISyn
  ( parsetop,
    ISyn (..),
    ISynFld (..),
    INewtypePayload (..),
    ViaInfo (..),
    Derive (..),
    Parser,

    -- * debug
    _testbody1,
  )
where

import Control.Monad
import Data.Void
import Text.Megaparsec (Parsec, many, (<|>))
import Text.Megaparsec qualified as M
import Text.Megaparsec.Char qualified as M
import Text.Megaparsec.Char.Lexer qualified as L

-- | parser type for the intermediate syntax
type Parser = Parsec Void String

-- intermediate syntax types

-- | intermediate representation of type definitions
-- parsed from the input syntax. can be a data type,
-- newtype, or type alias.
data ISyn
  = ISynData {isnam :: String, isflds :: [ISynFld]}
  | ISynNewtype {isnam :: String, isfld1 :: INewtypePayload}
  | ISynAlias {isnam :: String, isdest :: String}
  deriving (Show)

-- | payload for newtype definitions, which can either be
-- a field with a name and type, or just a type
data INewtypePayload
  = IField ISynFld
  | IType ViaInfo
  deriving (Show)

-- | field definition for data types and newtypes,
-- containing a field name and its type information
data ISynFld = ISynFld
  { isfldnam :: String,
    isfldtyp :: ViaInfo
  }
  deriving (Show)

-- | collection of type classes to derive for all defined types
newtype Derive = Derive
  { getderive :: [String]
  }
  deriving (Show)

-- | type information that may include a 'via' clause
data ViaInfo
  = Plain String
  | WithVia String String
  deriving (Show)

-- parser utilities
sc :: Parser ()
sc = L.space M.space1 (L.skipLineComment "--") (L.skipBlockComment "{-" "-}")

scnonl :: Parser ()
scnonl =
  L.space M.hspace1 (L.skipLineComment "--") (L.skipBlockComment "{-" "-}")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme scnonl

identifier :: Parser String
identifier = (:) <$> headchar <*> M.many bodychar
  where
    headchar = M.letterChar <|> M.char '_'
    bodychar = M.alphaNumChar <|> M.char '_' <|> M.char '\''

typechars :: Parser Char
typechars = M.alphaNumChar <|> M.oneOf "_'() ,:[]=.<>-|\""

untileol :: Parser Char -> Parser String
untileol p = M.try $ M.manyTill p (M.lookAhead (M.eof <|> void M.eol))

untilvia :: Parser String
untilvia =
  M.try $
    M.manyTill typechars $
      M.lookAhead (void M.eol <|> void (M.space1 *> M.string "via"))

-- Main parsers
parsetype :: Parser ViaInfo
parsetype =
  M.choice
    [ M.try $
        WithVia
          <$> lexeme untilvia
          <*> (lexeme (M.string "via") *> lexeme (untileol M.anySingle)),
      Plain <$> lexeme (untileol M.anySingle)
    ]

parsefield :: Parser ISynFld
parsefield = do
  isfldnam <- lexeme identifier
  void $ lexeme (M.string "::")
  isfldtyp <- parsetype
  pure ISynFld {isfldnam, isfldtyp}

indentblock :: Parser a -> Parser b -> Parser (a, [b])
indentblock p1 p2 = L.indentBlock sc do
  i <- p1
  pure $ L.IndentMany Nothing (pure . (i,)) p2

parsedata :: Parser ISyn
parsedata = do
  (isnam, isflds) <-
    indentblock
      (lexeme (M.string "data") *> lexeme identifier)
      parsefield
  pure ISynData {isnam, isflds}

parsenewtype :: Parser ISyn
parsenewtype = M.try parsenewtype1 <|> parsenewtype2
  where
    parsenewtype1 = do
      (isnam, flds) <-
        indentblock
          (lexeme (M.string "newtype") *> lexeme identifier)
          parsefield
      f <- case flds of
        [g] -> pure g
        _ -> fail "parsenewtype: a newtype must have exactly one field"
      pure ISynNewtype {isnam, isfld1 = IField f}
    parsenewtype2 = do
      void $ lexeme (M.string "newtype")
      isnam <- lexeme identifier
      typ <- parsetype
      pure ISynNewtype {isnam, isfld1 = IType typ}

parsealias :: Parser ISyn
parsealias = do
  void $ lexeme (M.string "type")
  isnam <- lexeme identifier
  isdest <- lexeme (untileol M.anySingle)
  pure ISynAlias {isnam, isdest}

parsederive :: Parser Derive
parsederive = do
  (_, !classes) <-
    indentblock
      (lexeme (M.string ".derive"))
      (many (lexeme identifier))
  pure $ Derive (join classes)

parsesyn :: Parser ISyn
parsesyn =
  M.try $
    M.choice
      [ M.try parsedata,
        M.try parsenewtype,
        M.try parsealias
      ]

parsetop :: Parser (Derive, [ISyn])
parsetop = do
  h <- parsederive
  decls <- parsesyn `M.sepEndBy1` M.space
  M.eof
  pure (h, decls)

-- test (when using GHCi)
_testbody1 :: String
_testbody1 =
  unlines
    [ ".coerce",
      "  mkpackdecls",
      "  mkunpackdecls",
      "",
      ".derive",
      "  Eq Ord Show Read",
      "  Generic Typeable Data",
      "",
      "data Person",
      "  age :: Int32 via Age",
      "  name :: String via VerifyLength 1 10 String",
      "  email :: String via VerifyEmail String",
      "",
      "newtype Age",
      "  getage :: Int32",
      ""
    ]
