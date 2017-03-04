{-# LANGUAGE DataKinds, ScopedTypeVariables, OverloadedStrings #-}
module Kantour.QuotesFetch.PageProcessor where

import Control.Monad.Logger
import qualified Data.IntMap as IM
import Control.Monad
import Kantour.QuotesFetch.Kcwiki
import Kantour.QuotesFetch.ShipDatabase
import Kantour.QuotesFetch.Types
import Control.Arrow
import Data.Maybe
import qualified Data.Text as T
import Data.Monoid

{-# ANN module ("HLint: ignore Eta reduce" :: String) #-}

modifyWithLog :: Monad m
                 => (Int -> Maybe v -> v -> m ())
                 -> IM.IntMap v -> Int -> (Maybe v -> m v) -> m (IM.IntMap v)
modifyWithLog doLog ms k modifyM = do
    let mOldV = IM.lookup k ms
    newV <- modifyM mOldV
    doLog k mOldV newV
    pure (IM.insert k newV ms)

processSeasonal :: forall m. MonadLogger m
                   => T.Text -> ShipDatabase -> PageContent 'Seasonal -> m ShipQuoteTable
processSeasonal src sdb qs = foldM go IM.empty (process <$> qs)
  where
    findMst = qlFindMasterId sdb . qlArchive
    findSituation = qaSituationId . qlArchive

    process :: QuoteLine -> (MasterId, (Int, QuoteLine))
    process = findMst &&& (findSituation &&& id)

    go :: ShipQuoteTable -> (MasterId, (Int, QuoteLine)) -> m ShipQuoteTable
    go cur (mstId,(sId,ql)) =
        modifyWithLog doLog cur mstId modifyQuoteTable
      where
        doLog _ _ _ =
            pure ()
            -- logInfoNS src $ "created: " <> T.pack (show mstId)

        modifyQuoteTable :: Maybe (IM.IntMap QuoteLine) -> m (IM.IntMap QuoteLine)
        modifyQuoteTable mOldTbl = modifyWithLog doLog2 curTbl sId (const (pure ql))
          where
            isSeasonal = qaIsNormalSeasonal . qlArchive
            doLog2 _ Nothing _ =
                pure ()
                -- logInfoNS src $ "inserted: " <> T.pack (show sId)
            doLog2 _ (Just oldQl) newQl = log' src msg
              where
                msg = "mstId=" <> T.pack (show mstId)
                      <> ", sId=" <> T.pack (show sId)
                      <> " replacing " <> getDesc oldQl <> " with " <> getDesc newQl
                log' = if not (isSeasonal oldQl) && isSeasonal newQl
                         then logInfoNS else logWarnNS
                getDesc x =  if isSeasonal x then "seasonal" else "normal"
            curTbl = fromMaybe IM.empty mOldTbl

qlFindMasterId :: ShipDatabase -> QuoteArchive -> MasterId
qlFindMasterId _ QARaw{} = error "qlFindMasterId: unexpected QARaw"
qlFindMasterId sdb (QANormal lId _ _) = findMasterId lId sdb
