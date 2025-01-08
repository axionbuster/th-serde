module Main where

import A.QQ
import Data.Data
import Data.Int
import GHC.Generics
import GHC.TypeLits

newtype VerifyLength (min :: Nat) (max :: Nat) a
  = VerifyLength a
  deriving (Eq, Ord, Show, Read, Generic, Typeable, Data)

newtype VerifyEmail a
  = VerifyEmail a
  deriving (Eq, Ord, Show, Read, Generic, Typeable, Data)

[serde|
.coerce
  mkpackdecls
  mkunpackdecls

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

main :: IO ()
main = putStrLn "Test suite not yet implemented"
