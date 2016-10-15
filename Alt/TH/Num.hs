module Alt.TH.Num
  ( 
    mkIntInstance
  ) where

import qualified Prelude as P
import Alt.Maybe
import Language.Haskell.TH

mkIntInstance :: Name -> Q [Dec]
mkIntInstance name = P.return
  [
    InstanceD Nothing [] (AppT (ConT (mkName "Semiring")) (ConT name)) 
      [fromPrelude x | x <- ["+", "*"]]
  , InstanceD Nothing [] (AppT (ConT (mkName "Monoring")) (ConT name))
      [
        FunD (mkName "one") [Clause [] (NormalB (LitE (IntegerL 1))) []]
      , FunD (mkName "zero") [Clause [] (NormalB (LitE (IntegerL 0))) []]
      ]
  , InstanceD Nothing [] (AppT (ConT (mkName "Ring")) (ConT name))
      [fromPrelude "-"]
  , InstanceD Nothing [] (AppT (ConT (mkName "Integral")) (ConT name))
      [fromPrelude x | x <- ["div", "mod", "divMod"]]       
  , InstanceD Nothing [] (AppT (ConT (mkName "FromInteger")) (ConT name))
      [fromPrelude "fromInteger"]
  , TySynInstD (mkName "Logic") (TySynEqn [ConT name] (ConT (mkName "Bool")))
  , InstanceD Nothing [] (AppT (ConT (mkName "Ordered")) (ConT name))
      [fromPrelude x | x <- ["<", ">", "<=", ">="]]
  , InstanceD Nothing [] (AppT (ConT (mkName "Decidable")) (ConT name))
      [fromPrelude "==", FunD (mkName "!=") [Clause [] (NormalB (VarE (mkName "P./="))) []]]
  ] 

fromPrelude :: P.String -> Dec
fromPrelude x = FunD (mkName x) [Clause [] (NormalB (VarE (mkName ("P." P.++ x)))) []]


