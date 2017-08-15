module Kantour.TH where

import Language.Haskell.TH
import Data.Maybe

genSubcommands :: Q Exp
genSubcommands = do
    (Just subcmdTypeName) <- lookupTypeName "Subcommand"
    ClassI _ ins <- reify subcmdTypeName
    (Just proxyTypeName) <- lookupTypeName "Proxy"
    (Just proxyValName) <- lookupValueName "Proxy"
    (Just esubValName) <- lookupValueName "ESub"
    let typNames = mapMaybe getTypes ins
        gen :: Name -> Exp
        gen n = AppE (ConE esubValName) inner
          where
            innerV = ConE proxyValName
            innerT = AppT (ConT proxyTypeName) (ConT n)
            inner = innerV `SigE` innerT
    pure (ListE (gen <$> typNames))
  where
    getTypes :: InstanceDec -> Maybe Name
    getTypes (InstanceD _ _ (AppT _ (ConT tyN)) _) = Just tyN
    getTypes _ = Nothing
