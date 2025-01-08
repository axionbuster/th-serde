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
  } deriving (Generic, Typeable, Data)
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

-- Implementation for derivetesttrait
derivetesttrait :: RunUserCoercion -> Q [Dec]
derivetesttrait RunUserCoercion {classnam, patnormal, appshadow} = do
  [d|
    instance TestTrait $(classnam) where
      testtrait $(patnormal) = testtrait ($(appshadow))
    |]
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
{-# LANGUAGE QuasiQuotes, TemplateHaskell #-}
```

2. Import the QuasiQuoter:
```haskell
import Data.Serde.QQ
```

3. Define your types using the `serde` QuasiQuoter as shown in the examples above.

## License

BSD-3-Clause
