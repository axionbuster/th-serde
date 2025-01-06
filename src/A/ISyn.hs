{-# OPTIONS_GHC -Wno-partial-fields #-}

-- | intermediate syntax for the parser
--
-- see "A.Syn" for the grammar
module A.ISyn
  ( parsetop,
    ISyn (..),
    ISynFld (..),
    INewtypePayload (..),
    ViaInfo (..),
    CoercePair (..),
    CoerceHeader (..),
    Derive (..),
  )
where

import Control.Monad
import Data.Void
import Text.Megaparsec (Parsec, many, (<|>))
import Text.Megaparsec qualified as M
import Text.Megaparsec.Char qualified as M
import Text.Megaparsec.Char.Lexer qualified as L

type Parser = Parsec Void String

-- Intermediate syntax types
data ISyn
  = ISynData {isnam :: String, isflds :: [ISynFld]}
  | ISynNewtype {isnam :: String, isfld1 :: INewtypePayload}
  | ISynAlias {isnam :: String, isdest :: String}
  deriving (Show)

data INewtypePayload
  = IField ISynFld
  | IType ViaInfo
  deriving (Show)

data ISynFld = ISynFld
  { isfldnam :: String,
    isfldtyp :: ViaInfo
  }
  deriving (Show)

data CoercePair = CoercePair
  { cpclass :: String,
    cpfun :: String
  }
  deriving (Show)

newtype CoerceHeader = CoerceHeader
  { getcoerceheader :: [CoercePair]
  }
  deriving (Show)

newtype Derive = Derive
  { getderive :: [String]
  }
  deriving (Show)

data ViaInfo
  = Plain String
  | WithVia String String
  deriving (Show)

-- Parser utilities
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
typechars = M.alphaNumChar <|> M.oneOf "_'() ,:[]=.<>-|"

untileol :: Parser Char -> Parser String
untileol p = M.manyTill p (M.lookAhead (M.eof <|> void M.eol))

untilvia :: Parser String
untilvia =
  M.manyTill typechars $
    M.lookAhead (M.try (void M.eol <|> void (M.space1 *> M.string "via")))

-- Main parsers
parsetype :: Parser ViaInfo
parsetype =
  M.choice
    [ WithVia
        <$> M.try (lexeme untilvia)
        <*> M.try (lexeme (M.string "via") *> lexeme (untileol M.anySingle)),
      Plain <$> untileol M.anySingle
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
      (isnam, [isfld]) <-
        indentblock
          (lexeme (M.string "newtype") *> lexeme identifier)
          parsefield
      pure ISynNewtype {isnam, isfld1 = IField isfld}

    parsenewtype2 = do
      (isnam, [typ]) <-
        indentblock
          (lexeme (M.string "newtype") *> lexeme identifier)
          parsetype
      pure ISynNewtype {isnam, isfld1 = IType typ}

parsealias :: Parser ISyn
parsealias = do
  void $ lexeme (M.string "type")
  isnam <- lexeme identifier
  isdest <- lexeme (untileol M.anySingle)
  pure ISynAlias {isnam, isdest}

parsecoerce :: Parser CoerceHeader
parsecoerce = do
  (_, pairs) <-
    indentblock
      (lexeme (M.string ".coerce"))
      (many (lexeme identifier))
  pure $
    CoerceHeader
      [CoercePair (unwords $ init ws) (last ws) | ws <- pairs]

parsederive :: Parser Derive
parsederive = do
  (_, classes) <-
    indentblock
      (lexeme (M.string ".derive"))
      (many (lexeme identifier))
  pure $ Derive (join classes)

parseheader :: Parser (CoerceHeader, Derive)
parseheader = (,) <$> parsecoerce <*> parsederive

parsesyn :: Parser ISyn
parsesyn = M.choice [parsedata, parsenewtype, parsealias]

parsetop :: Parser ((CoerceHeader, Derive), [ISyn])
parsetop = (,) <$> parseheader <*> (M.space *> many (M.try parsesyn))

-- Test (when using GHCi)
_testbody1 :: String
_testbody1 =
  unlines
    [ ".coerce",
      "  Pack mkpackdecls",
      "  Unpack mkunpackdecls",
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
