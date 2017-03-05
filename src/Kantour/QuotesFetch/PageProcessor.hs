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
{-# ANN module ("HLint: ignore Avoid lambda" :: String) #-}
{-# ANN module ("HLint: ignore Use unless" :: String) #-}

modifyWithLog ::
    Monad m
    => (Int -> Maybe v -> v -> m ())
    -> IM.IntMap v -> Int -> (Maybe v -> m v) -> m (IM.IntMap v)
modifyWithLog doLog ms k modifyM = do
    let mOldV = IM.lookup k ms
    newV <- modifyM mOldV
    doLog k mOldV newV
    pure (IM.insert k newV ms)

-- SQT: ShipQuoteTable
loggedSQTInsert ::
    forall m. MonadLogger m
    => T.Text -> ShipQuoteTable -> [(MasterId,(Int, QuoteLine))] -> m ShipQuoteTable
loggedSQTInsert src initSqt xs = foldM go initSqt xs
  where
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
            doLog2 = quoteLineReplacingLog src mstId
            curTbl = fromMaybe IM.empty mOldTbl

processSeasonal ::
    forall m. MonadLogger m
    => T.Text -> ShipDatabase -> PageContent 'Seasonal -> m ShipQuoteTable
processSeasonal src sdb qs =
    loggedSQTInsert src IM.empty (indexedQuoteLine sdb <$> qs)

indexedQuoteLine :: ShipDatabase -> QuoteLine -> (MasterId, (Int, QuoteLine))
indexedQuoteLine sdb = findMst &&& (findSituation &&& id)
  where
    findMst = qlFindMasterId sdb . qlArchive
    findSituation = qaSituationId . qlArchive

qlFindMasterId :: ShipDatabase -> QuoteArchive -> MasterId
qlFindMasterId _ QARaw{} = error "qlFindMasterId: unexpected QARaw"
qlFindMasterId sdb (QANormal lId _ _) = libIdToMasterId sdb lId

processRegular ::
    forall m. MonadLogger m
    => T.Text -> ShipDatabase -> PageContent 'ShipInfo -> m ShipQuoteTable
processRegular src sdb (TR tbs,sectionsRaw) = do
    -- select section that has contents in it
    let sections = filter valid sectionsRaw
          where
            valid (Header _ secName,xs) = not (null xs) && secName /= "语音存档"
        mstIdTable = (map . second) (libIdToMasterId sdb) tbs
        processSection :: (Header, [QuoteLine]) -> m [(MasterId,(Int,QuoteLine))]
        processSection (Header _ secName, qls) = do
            when (secName `notElem` ["报时","时报"]) $ do
                let mMstId = lookup secName mstIdTable
                case mMstId of
                    Nothing ->
                        logWarnNS src
                        $ "section '" <> T.pack secName <> "' not found in tabber"
                    Just mstId -> do
                        let qMstIds = qlFindMasterId sdb . qlArchive <$> qls
                            inconsistents = filter (/= mstId) qMstIds
                        when (not (null inconsistents)) $
                          logWarnNS src $ "found inconsistent mst ids: "
                          <> T.pack (show inconsistents)
                          <> " under section '" <> T.pack secName <> "'"
            pure (indexedQuoteLine sdb <$> qls)
    xs <- concat <$> mapM processSection sections
    loggedSQTInsert src IM.empty xs

-- SQT: ShipQuoteTable
loggedSQTUnion ::
    forall m. MonadLogger m
    => T.Text -> ShipQuoteTable -> [(MasterId, IM.IntMap QuoteLine)] -> m ShipQuoteTable
loggedSQTUnion src initSqt xs = foldM go initSqt xs
  where
    go :: ShipQuoteTable -> (MasterId, IM.IntMap QuoteLine) -> m ShipQuoteTable
    go cur (mstId,subM) =
        modifyWithLog doLog cur mstId modifyQuoteTable
      where
        doLog _ _ _ =
            pure ()
            -- logInfoNS src $ "created: " <> T.pack (show mstId)

        sPairs = IM.toList subM
        modifyQuoteTable :: Maybe (IM.IntMap QuoteLine) -> m (IM.IntMap QuoteLine)
        modifyQuoteTable mOldTbl = foldM go2 curTbl sPairs
          where
            curTbl = fromMaybe IM.empty mOldTbl
            go2 :: IM.IntMap QuoteLine -> (Int, QuoteLine) -> m (IM.IntMap QuoteLine)
            go2 curTbl' (sId,ql) = modifyWithLog doLog2 curTbl' sId (const (pure ql))
              where
                doLog2 = quoteLineReplacingLog src mstId

quoteLineReplacingLog ::
    MonadLogger m
    => T.Text -> MasterId -> Int -> Maybe QuoteLine -> QuoteLine -> m ()
quoteLineReplacingLog _ _ _ Nothing _ = pure ()
    -- logInfoNS src $ "inserted: " <> T.pack (show sId)
quoteLineReplacingLog src mstId sId (Just oldQl) newQl = log' src msg
  where
    isSeasonal = qaIsNormalSeasonal . qlArchive
    msg = "mstId=" <> T.pack (show mstId)
          <> ", sId=" <> T.pack (show sId)
          <> " replacing " <> getDesc oldQl <> " with " <> getDesc newQl
    log' = if not (isSeasonal oldQl) && isSeasonal newQl
             then logInfoNS else logWarnNS
    getDesc x =  if isSeasonal x then "seasonal" else "normal"
