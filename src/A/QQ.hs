module A.QQ (serde) where

import A.Syn
import A.TH
import A.Type
import Language.Haskell.TH as TH
import Language.Haskell.TH.Quote as TH

-- | quasi-quoter for serde
serde :: QuasiQuoter
serde =
  QuasiQuoter
    { quoteExp = error "serde: quoteExp not implemented",
      quotePat = error "serde: quotePat not implemented",
      quoteType = error "serde: quoteType not implemented",
      quoteDec = \s -> case parse s of
        Left e -> fail e
        Right p -> pure $ concat [gendecs t | t <- declarations p]
    }
