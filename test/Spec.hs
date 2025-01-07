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
  Pack mkpackdecls
  Unpack mkunpackdecls

.derive
  Eq Ord Show Read
  Generic Typeable Data

data Person
  age :: Int32 via Age
  name :: String via (VerifyLength 1 10 String)
  email :: String via (VerifyEmail String)

newtype Age
  getage :: Int32

newtype Great String

type MyAlias String
|]

main :: IO ()
main = putStrLn "Test suite not yet implemented"
