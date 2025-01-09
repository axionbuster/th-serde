-- | The main stable interface for the th-serde library, providing QuasiQuoters
-- for defining data types with separated serialization and validation logic.
-- Other modules are considered internal and may change without notice.
--
-- = Overview
--
-- This library helps separate your core data models from their serialization and
-- validation logic. While newtypes are commonly used for this purpose, they become
-- unwieldy with complex data types, especially when dealing with multiple fields
-- that need validation or custom serialization.
--
-- = How It Works
--
-- Given a data type definition with \'via\' annotations, th-serde:
--
-- 1. Generates your core data type with clean, simple fields
-- 2. Creates a \"shadow\" type with the validation/serialization wrappers
-- 3. Provides machinery to convert between them using 'Data.Coerce.coerce'
--
-- Important: Shadow types are generated without any type class implementations.
-- You must implement all needed type classes for shadow types using 'runuserprep'.
-- The QuasiQuoter never automatically derives any instances for shadow types.
--
-- = Example
--
-- Here's a complete example:
--
-- @
-- [serde|
-- .derive
--   Eq Ord Show Read
--
-- -- Your core data type with validation annotations
-- -- Suppose Age, VerifyLength, and VerifyEmail are defined elsewhere
-- data Person
--   age :: Int32 via Age                     -- Validate using Age newtype
--   name :: String via VerifyLength 1 10     -- Must be 1-10 chars
--   email :: String via VerifyEmail          -- Must be valid email
-- |]
-- @
--
-- This generates:
--
-- @
-- -- Your clean business model
-- data Person = Person
--   { age :: Int32    -- Clean types without validation wrappers
--   , name :: String
--   , email :: String
--   } deriving (Eq, Ord, Show, Read)
--
-- -- Auto-generated shadow type for validation
-- data Person\_\_ = Person\_\_
--   { age__ :: Age               -- Fields use validation wrappers
--   , name__ :: VerifyLength 1 10
--   , email__ :: VerifyEmail
--   }
-- @
--
-- = Usage Pattern
--
-- 1. Define your types using the 'serde' QuasiQuoter
-- 2. Call 'runuserprep' to implement basic type classes
-- 3. Call 'runusercoercion' to implement validation/serialization
--
-- = Syntax Reference
--
-- == Deriving Classes
--
-- Use @.derive@ at the start to specify which classes to derive:
--
-- @
-- .derive
--   Eq Ord Show Read  -- These will be derived for all types except shadow types
-- @
--
-- == Data Types
--
-- @
-- data Person                    -- Regular data type
--   field :: Type via Validator  -- Field with validation
--   plain :: Type                -- Field without validation
-- @
--
-- == Newtypes
--
-- Unlike data types, newtypes using 'via' (either in field or type position)
-- will use GHC's @DerivingVia@ mechanism directly instead of creating shadow types.
-- This requires the @DerivingVia@ language extension to be enabled.
--
-- @
-- newtype Age        -- Simple newtype without via
--   value :: Int32   -- Regular field, no shadow type created
--
-- newtype Number Double
--
-- newtype Validated Int32 via Check  -- With validation
-- -- ^ Generates:
-- --   newtype Validated = Validated Int32
-- --     deriving (...) via Check
--
-- newtype Name       -- With record syntax + via
--   getName :: String via Verify
-- -- ^ Generates:
-- --   newtype Name = Name { getName :: String }
-- --     deriving (...) via Verify
-- @
--
-- == Type Aliases
--
-- Type aliases do not participate in the derivation process and are not shadowed.
--
-- @
-- type EmailStr String
-- @
--
-- == Examples
--
-- Examples can be found in the test suite.
module Data.Serde.QQ
  ( serde,
    RunUserCoercion (..),
    runusercoercion,
    runuserprep,
  )
where

import Data.Serde.Internal.TH
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
