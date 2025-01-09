# th-serde

A lightweight Haskell library for separating data structure definitions from their serialization and validation logic using Template Haskell and QuasiQuotes.

## Problem

When working with data types that need validation or custom serialization, you often end up with newtypes wrapping your fields. While this works for simple cases, it becomes problematic with complex data types because:

1. Data types with identical or coercible fields cannot be coerced automatically
2. Mixing business logic with validation/serialization concerns makes code harder to maintain
3. Lots of boilerplate when implementing type class instances for validation/serialization

## Solution

th-serde provides a QuasiQuoter that lets you define your data types with a clean syntax while automatically generating shadow types that handle validation and serialization. For example:

```haskell
[serde|
.derive
  Eq Ord Show Read
  Generic Typeable Data

data Person
  age :: Int32 via Age
  name :: String via VerifyLength 1 10 String
  email :: String via VerifyEmail String

newtype Age
  getage :: Int32
|]
```

This generates:

```haskell
-- Your clean business model
data Person = Person
  { age :: Int32
  , name :: String
  , email :: String
  } deriving (Eq, Ord, Show, Read, Generic, Typeable, Data)

-- Auto-generated shadow type for validation/serialization
data Person__ = Person__
  { age__ :: Age
  , name__ :: VerifyLength 1 10 String
  , email__ :: VerifyEmail String
  }
```

## Features

- Clean syntax for defining data types with validation/serialization concerns
- Automatic generation of shadow types
- Support for custom type class derivation through coercion
- Works with both regular data types and newtypes
- Type alias support

## Custom Type Class Derivation

You can easily derive custom type classes that work with your shadow types. Here's an example:

```haskell
-- Define a type class
class TestTrait a where
  testtrait :: a -> Set String
  default testtrait :: (Generic a, GTestTrait (Rep a)) => a -> Set String
  testtrait = gtesttrait . from

-- Generate instances for your types
runusercoercion derivetesttrait [''TestTrait]

-- Make two implementations:
-- input to runuserprep (here, preptype):
-- for all data types, including their shadow counterparts and newtypes
-- but not type aliases
--
-- input to runusercoercion (here, derivetesttrait):
-- for the shadow counterparts of data types; not newtypes, type aliases
-- or data types with no 'via' fields

preptype :: Q TH.Type -> Q [Dec] -- for all types
preptype t = do
  [d|
    deriving instance Show $t

    deriving instance Generic $t
    |]

derivetesttrait :: RunUserCoercion -> Q [Dec] -- only for shadow data
derivetesttrait RunUserCoercion {..} = do
  -- borrow implementation from shadow type
  --
  -- datatyp: data type name
  -- patnormal: deconstructs user data
  -- appshadow: constructs shadow data
  --
  -- flow:
  --  patnormal ->
  --    appshadow [create shadow data] ->
  --      (class method) (borrow implementation from shadow)
  [d|
    instance TestTrait $(datatyp) where
      testtrait $(patnormal) = testtrait ($(appshadow))
    |]

  -- of course, opposite flow is possible as well
  --
  -- see patshadow and appshadow (Data.Serde.Internal.TH)
  -- (reexported by Data.Serde.QQ)
  --
  -- suggested flow:
  --  (class method) ->
  --    patshadow [create shadow data] ->
  --      appnormal (reconstruct real data)
```

## Installation

Add to your project's dependencies:

```yaml
dependencies:
  - th-serde
```

## Usage

1. Enable the necessary language extensions:
```haskell
-- GHC2021+
{-# LANGUAGE QuasiQuotes, TemplateHaskell #-}
{-# LANGUAGE DataKinds, DerivingStrategies, DerivingVia #-}

-- Haskell2010+
-- may need to enable StandaloneDeriving and more
```

2. Import the QuasiQuoter:
```haskell
import Data.Serde.QQ
```

3. Define your types using the `serde` QuasiQuoter as shown in the examples above.

## License

BSD-3-Clause
