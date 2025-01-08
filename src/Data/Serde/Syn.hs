{-# OPTIONS_GHC -Wno-partial-fields #-}

-- | define syntax
module Data.Serde.Syn
  ( -- * parsing
    Parsed (..),
    parse,
    parsetypeexts,

    -- * syntax
    Syn (..),
    SynFld (..),
    ViaInfo (..),

    -- * re-export
    Name,
    Type,
  )
where

import Data.Serde.ISyn
import Language.Haskell.Exts.Simple.Extension
import Language.Haskell.Exts.Simple.Parser hiding (parse)
import Language.Haskell.Exts.Simple.Syntax
import Text.Megaparsec qualified as M

-- | parsed data
data Parsed = Parsed
  { declarations :: [Syn],
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

-- | go from type string to 'Exts'.'Type'
--
-- you need to use a function from "Data.Serde.Type" to convert this to a TH.'TH.Type'
parsetypeexts :: String -> Type
parsetypeexts s = case parseTypeWithMode pm1 s of
  ParseOk x -> x
  ParseFailed _ e -> error $ "parsetypeexts: " ++ show e

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
    ISynAlias n d -> SynAlias (name n) (parsetypeexts d)
  where
    name = Ident
    fromderive = fmap name . getderive
    fromisynfld (ISynFld n t) = case fromviainfo t of
      Left v -> SynFld (name n) v Nothing
      Right (s, u) -> SynFld (name n) s (Just u)
    fromviainfo (Plain t) = Left (parsetypeexts t)
    fromviainfo (WithVia t s) = Right (parsetypeexts t, parsetypeexts s)

-- | parse quasi-quoted syntax
parse :: String -> Either String Parsed
parse s =
  let res = M.runParser (parsetop <* M.eof) "" s
   in case res of
        Right (de, fmap (fromisyn de) -> ds) ->
          Right $ Parsed ds de
        Left e -> Left $ M.errorBundlePretty e
