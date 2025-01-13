# Changelog for `th-serde`

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to the
[Haskell Package Versioning Policy](https://pvp.haskell.org/).

## Unreleased

## 0.1.3.0 - 2025-01-11

- Changed signature of `runusercoercion` in internal TH module
- The function now takes three parameters:
  1. Function to generate coercions between shadow/regular data types
  2. Function to derive coercions for shadow data, regular data with no shadows, and newtypes
  3. List of type class names to prepare

## 0.1.2.2 - 2025-01-10

- Hotfix for a bug affecting parsing of 'via'

## 0.1.2.1 - 2025-01-09

- Minor/internal changes (documentation, etc.)

## 0.1.2.0 - 2025-01-09

- Properly implemented derive mechanism
- Changed usage pattern: user must
  1. Use `[serde| ... |]`
  2. Call Template Haskell function `runuserprep` (__NEW__)
  3. Call Template Haskell function `runusercoercion`
- `runuserprep` implements derives for all types except type aliases.
  "all types" includes both data types and their shadows

## 0.1.1.0 - 2025-01-09

- Reorganized modules to better indicate internal vs stable APIs
- Moved implementation modules to Data.Serde.Internal.\*
- Improved documentation

## 0.1.0.0 - 2025-01-08

- Initial release of the library
- `serde` QuasiQuoter for defining data types with validation/serialization
- Automatic shadow type generation
- Custom type class derivation support through coercion
- Support for regular data types and newtypes
- Type alias support
- An example

### Dependencies
- Requires GHC 9.8 or later
- Uses template-haskell 2.21.0
