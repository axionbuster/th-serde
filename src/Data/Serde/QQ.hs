-- | quasi-quotation for th-serde
--
-- a lightweight syntax for defining data types, newtypes, and type aliases
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
-- @
-- .derive
--  Eq Ord Show Read
--  Generic Typeable Data
-- @
--
-- __ @.derive@ __
--
-- @.derive@ defines which type classes to derive for all types defined in
-- the module
--
-- they are not given in the shadow type, but in the data type
--
-- shadow types only implement: 'Generic', 'Typeable', and 'Data'
module Data.Serde.QQ (serde, RunUserCoercion (..), runusercoercion) where

import Data.Serde.TH
import Language.Haskell.TH.Quote as TH

-- | quasi-quoter for th-serde
serde :: QuasiQuoter
serde =
  QuasiQuoter
    { quoteExp = error "serde: quoteExp not implemented",
      quotePat = error "serde: quotePat not implemented",
      quoteType = error "serde: quoteType not implemented",
      quoteDec = runqq1
    }
