module Main (main) where

import Data.Serde.QQ
import Data.Data
import Data.Int
import GHC.Generics
import GHC.TypeLits
import TestTrait

newtype VerifyLength (min :: Nat) (max :: Nat) a
  = VerifyLength a
  deriving stock (Eq, Ord, Show, Read, Generic, Typeable, Data)

newtype VerifyEmail a
  = VerifyEmail a
  deriving stock (Eq, Ord, Show, Read, Generic, Typeable, Data)

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

newtype Great String

type MyAlias VerifyEmail String

-- this should not shadow
data Noshadow
  noshadow :: Int32
  anotherfield :: String
|]

runusercoercion derivetesttrait [''TestTrait]

main :: IO ()
main = do
  -- suppose we get this from an external source
  let person1 = Person {age = 10, name = "John", email = "a@a.com"}
  -- let's see...
  print $ testtrait person1
