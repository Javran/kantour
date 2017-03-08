{-# LANGUAGE DataKinds, ScopedTypeVariables, OverloadedStrings, KindSignatures, GADTs #-}
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
import Data.Foldable
import Data.Monoid
import Data.List
import Data.Char

{-# ANN module ("HLint: ignore Eta reduce" :: String) #-}
{-# ANN module ("HLint: ignore Avoid lambda" :: String) #-}
{-# ANN module ("HLint: ignore Use unless" :: String) #-}

modifyWithLog ::
    (Monad m, Eq v)
    => (Int -> Maybe v -> v -> m ())
    -> IM.IntMap v -> Int -> (Maybe v -> m v) -> m (IM.IntMap v)
modifyWithLog doLog ms k modifyM = do
    let mOldV = IM.lookup k ms
    newV <- modifyM mOldV
    case mOldV of
        Nothing ->
            doLog k mOldV newV
        Just oldV ->
            if oldV == newV
              then pure ()
              else doLog k mOldV newV
    pure (IM.insert k newV ms)

removeEmptyQuoteLines :: MonadLogger m => [QuoteLine] -> m [QuoteLine]
removeEmptyQuoteLines xs = do
    mapM_ doLog xsEmpty
    pure xsGood
  where
    (xsGood, xsEmpty) = partition notEmpty xs
    notEmpty ql = case qlTextSCN ql of
        Nothing -> False
        Just "" -> False
        Just content@(_:_) -> not (all isSpace content)

    doLog ql = logWarnNS "removeEmptyQuoteLines" desc
      where
        desc = case qlArchive ql of
          QANormal lId sId extra ->
              "LibId: " <> T.pack lId
               <> ", SituationId: " <> T.pack (show sId)
               <> if extra == "" then "" else ", Extra: " <> T.pack extra
          QARaw r -> "Raw: " <> T.pack (show r)

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
        modifyQuoteTable mOldTbl = modifyWithLog doLog2 curTbl sId compareAndUpdate
          where
            doLog2 = quoteLineReplacingLog src mstId
            curTbl = fromMaybe IM.empty mOldTbl

            compareAndUpdate :: Maybe QuoteLine -> m QuoteLine
            compareAndUpdate Nothing = pure ql
            compareAndUpdate (Just oldQl) =
                case (qlGetExtraIndex oldQl, qlGetExtraIndex ql) of
                    (Just oldInd, Just newInd) ->
                        let l = logInfoNS "compareAndUpdate"
                        in if newInd <= oldInd
                             then do
                               l "replacing tgt with src"
                               l $ "src: " <> T.pack (show (qlArchive ql))
                               l $ "tgt: " <> T.pack (show (qlArchive oldQl))
                               pure ql
                             else do
                               l "update skipped because tgt is newer"
                               l $ "src: " <> T.pack (show (qlArchive ql))
                               l $ "tgt: " <> T.pack (show (qlArchive oldQl))
                               pure oldQl
                    _ -> pure ql

processSeasonal ::
    forall m. MonadLogger m
    => T.Text -> ShipDatabase -> [QuoteLine] -> m ShipQuoteTable
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

-- this function is just for showing it is possible to unify regular processing
-- and seasonal processing into one function, but this isn't really useful,
-- because through the process we are doing modifications to some of the output
-- making re-wraping data using a proper type an unnecessary task.
processPage ::
    forall m (k :: PageType) . MonadLogger m
    => T.Text -> ShipDatabase -> PageContent k -> m ShipQuoteTable
processPage src sdb pc = case pc of
    PgShipInfo trs secs ->
        processRegular src sdb (trs,secs)
    PgSeasonal qs ->
        processSeasonal src sdb qs

processRegular ::
    forall m. MonadLogger m
    => T.Text -> ShipDatabase -> (TabberRows,[(Header,[QuoteLine])]) -> m ShipQuoteTable
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

reapplyQuoteLines :: ShipDatabase -> ShipQuoteTable -> ShipQuoteTable
reapplyQuoteLines sdb srcSQT = foldl' go IM.empty origins
  where
    origins = getOrigins sdb
    go :: ShipQuoteTable -> MasterId -> ShipQuoteTable
    go initSQT originMstId = applyAlongChain originMstId IM.empty initSQT
      where
        applyAlongChain curMstId prevQls prevSQT = case mNextMstId of
            Nothing -> curSQT
            Just nextMstId ->
                maybe
                  -- continue if not found
                  (applyAlongChain nextMstId curQls curSQT)
                  -- stop if found
                  (const curSQT)
                  (IM.lookup nextMstId curSQT)
          where
            mNextMstId = nextRemodel sdb curMstId
            curSQT = IM.insert curMstId curQls prevSQT
            curQls = maybe
                prevQls
                (\nowQls -> IM.union nowQls prevQls)
                (IM.lookup curMstId srcSQT)