module TestTrait (TestTrait (..), derivetesttrait, preptype) where

import Data.Int
import Data.Serde.QQ
import Data.Set (Set, singleton)
import GHC.Generics
import Language.Haskell.TH as TH

class (Show a) => TestTrait a where
  testtrait :: a -> Set String -- collect Show values itself & its fields
  default testtrait :: (Generic a, GTestTrait (Rep a)) => a -> Set String
  testtrait = gtesttrait . from

instance TestTrait Int32 where
  testtrait = singleton . show

instance TestTrait String where
  testtrait = singleton

class GTestTrait f where
  gtesttrait :: f a -> Set String

instance GTestTrait U1 where
  gtesttrait _ = mempty

instance (Show c) => GTestTrait (K1 i c) where
  gtesttrait (K1 x) = singleton (show x)

instance (GTestTrait a) => GTestTrait (M1 i c a) where
  gtesttrait (M1 x) = gtesttrait x

instance (GTestTrait a, GTestTrait b) => GTestTrait (a :+: b) where
  gtesttrait (L1 x) = gtesttrait x
  gtesttrait (R1 x) = gtesttrait x

instance (GTestTrait a, GTestTrait b) => GTestTrait (a :*: b) where
  gtesttrait (x :*: y) = gtesttrait x <> gtesttrait y

preptype :: Q TH.Type -> Q [Dec]
preptype t = do
  [d|
    deriving instance Show $t

    deriving instance Generic $t
    |]

derivetesttrait :: RunUserCoercion -> Q [Dec]
derivetesttrait RunUserCoercion {..} = do
  [d|
    instance TestTrait $(datatyp) where
      testtrait $(patnormal) = testtrait ($(appshadow))
    |]
