{-|
Module: Kantour.QuotesFetch.Kcwiki
Description: Kcwiki- or MediaWiki-related structures

Kcwiki- or MediaWiki-related structures
Note that all representations in this module are by no means complete
but are extended sufficiently to serve the purpose.
|-}
{-# LANGUAGE TupleSections, GADTs, DeriveGeneric, DeriveAnyClass, DataKinds, TypeFamilies #-}
module Kantour.QuotesFetch.Kcwiki
  ( Template(..)

  , TemplateArg
  , fromRawTemplate

  , templateAsText

  , Header(..)

  , QuoteLine(..)

  , pprTabberRows

  , Component(..)
  , Page(..)

  , QuoteArchive(..)
  , mkQuoteArchive
  , qaIsNormalSeasonal

  , PageType(..)
  , PageContent

  , ShipQuoteTable
  ) where

import Kantour.QuotesFetch.Types
import Data.Maybe
import Data.Typeable
import Data.Char
import Control.Monad
import Text.PrettyPrint.HughesPJClass hiding (char)
import Kantour.QuotesFetch.Quotes (kcwikiTable)
import Text.Megaparsec
import Text.Megaparsec.String
import GHC.Generics
import Control.DeepSeq
import qualified Data.IntMap as IM
{-|
  Kcwiki template.

  __INVARIANT__: 'TplUnknown' should only be used when other constructors does not fit.
-}
data Template
  = TplQuote
    -- https://zh.kcwiki.moe/wiki/Template:%E5%8F%B0%E8%AF%8D%E7%BF%BB%E8%AF%91%E8%A1%A8
    { tQuoteLine :: QuoteLine }
  | TplQuoteListBegin
    -- https://zh.kcwiki.moe/wiki/Template:%E5%8F%B0%E8%AF%8D%E7%BF%BB%E8%AF%91%E8%A1%A8/%E9%A1%B5%E5%A4%B4
    { tIsSeasonal :: Bool
    }
  | TplEnd
    -- https://zh.kcwiki.moe/wiki/Template:%E9%A1%B5%E5%B0%BE
  | TplLang
    -- https://zh.kcwiki.moe/wiki/Template:Lang
    { tLang :: Maybe String
    , tContent :: Maybe String
    }
  | TplShipInfo
    -- https://zh.kcwiki.moe/wiki/Template:%E8%88%B0%E5%A8%98%E8%B5%84%E6%96%99
    { tLibId :: Maybe LibraryId
    }
  | TplUnknown
    -- general representation of not yet recognized templates
    { tName :: String
    , tArgs :: [TemplateArg]
    }
  deriving (Eq, Show, Typeable, Generic, NFData)

type TemplateArg =
  ( Maybe String -- optional key
  , String -- value
  )

data Header = Header
  { hdLevel :: Int
  , hdContent :: String
  } deriving (Eq, Show, Generic, NFData)

data QuoteLine = QL
  { qlArchive :: QuoteArchive
  , qlSituation :: Maybe String
  , qlTextJP :: Maybe String
  , qlTextSCN :: Maybe String
  , qlIsSeasonal :: Bool
    -- followings are for seasonal only
  , qlShipName :: Maybe String
  , qlShipId :: Maybe LibraryId
  } deriving (Eq, Show, Generic, NFData)

data QuoteArchive
  = QANormal
    { qaLibId :: LibraryId
    , qaSituationId :: Int
    , qaExtra :: String
    }
    -- for special archive names,
    -- or those that we fail to parse.
    -- we just keep the raw text
  | QARaw String
    deriving (Eq, Show, Generic, NFData)

qaIsNormalSeasonal :: QuoteArchive -> Bool
qaIsNormalSeasonal (QANormal { qaExtra = e }) = not (null e)
qaIsNormalSeasonal _ = False

{-|
  ignore 'Nothing's and convert rest of a template
  argument pairs into a table for further looking up
-}
tArgsToTable :: [TemplateArg] -> [(String,String)]
tArgsToTable = concatMap (\(mk,v) -> maybeToList ((,v) <$> mk))

{-|
  smart 'Template' constructor.
-}
fromRawTemplate :: String -> [TemplateArg] -> Template
fromRawTemplate tpName tpArgs = case tpName of
    "台词翻译表/页头" ->
        TplQuoteListBegin (lkup "type" == Just "seasonal")
    "台词翻译表" -> TplQuote (fromRawQuoteLine tArgTbl)
    "页尾" -> TplEnd
    "lang" -> TplLang (safeInd 0) (safeInd 1)
    "舰娘资料" -> TplShipInfo (lkup "编号")
    _ -> TplUnknown tpName tpArgs
  where
    tArgTbl = tArgsToTable tpArgs
    lkup :: String -> Maybe String
    lkup k = lookup k tArgTbl
    l = length tpArgs
    safeInd :: Int -> Maybe String
    safeInd i = do
        guard (i < l)
        pure (snd (tpArgs !! i))

{-|
  render template as text, only @lang@ template produces
  non-empty result.
-}
templateAsText :: Template -> String
templateAsText (TplLang _ (Just content)) = content
templateAsText _ = ""

fromRawQuoteLine :: [(String, String)] -> QuoteLine
fromRawQuoteLine xs = QL
    { qlArchive =
          maybe
            (error "fromRawQuoteLine: missing archive name")
            mkQuoteArchive
            (lkup "档名")
    , qlSituation = lkup "场合"
    , qlTextJP = lkup "日文台词"
    , qlTextSCN = lkup "中文译文"
    , qlIsSeasonal = lkup "type" == Just "seasonal"
    , qlShipName = lkup "舰娘名字"
    , qlShipId = lkup "编号"
    }
  where
    lkup k = lookup k xs

instance Pretty Header where
    pPrint h =
        hang (text "Header") 2 $
          vcat [ text "level:" <+> int (hdLevel h)
               , text "content:" <+> text (hdContent h)
               ]

instance Pretty Template where
    pPrint t = case t of
        TplQuote ql -> pPrint ql
        TplQuoteListBegin s ->
            text "ListBegin"
            <> if s then text " (Seasonal)" else empty
        TplEnd -> text "ListEnd"
        TplLang _ content ->
            hang (text "Lang") 2
            $ maybe empty text content
        TplShipInfo m ->
            hang (text "ShipInfo") 2
            $ maybe empty (\x -> text "Id:" <+> text x) m
        TplUnknown n ps ->
            let ps' = zip ps [0..]
                pprTA ((mK,v),ind) = case mK of
                    Just k -> text k <> text ": " <> text v
                    Nothing -> text "Ind" <+> int ind <> text ": " <> text v
            in hang (text "GTemplate" <+> text n) 2
               $ vcat (map pprTA ps')
instance Pretty QuoteLine where
    pPrint t =
        hang (text "QuoteLine" <+> desc) 2 $
          vcat (mapMaybe ((\(k,v) -> (text k <> text ": " <> text v)) <$>) ms)
      where
        -- TODO: Pretty for Quote Archive
        desc = text "Arch: " <> text (show (qlArchive t))
            <> (if qlIsSeasonal t
                  then text " (Seasonal)"
                  else empty)
        ms = [ ("Situation",) <$> qlSituation t
             , ("JP",) <$> qlTextJP t
             , ("SCN",) <$> qlTextSCN t
             , ("SName",) <$> qlShipName t
             , ("SLibId",) <$> qlShipId t
             ]

newtype Page = Page [Component]

-- non-divisible components of a page
data Component
  = CHeader Header
  | CTabber TabberRows
  | CTemplate Template
    deriving (Generic, NFData)

instance Pretty Page where
    pPrint (Page cs) = vcat (map f cs)
      where
        f :: Component -> Doc
        f c = case c of
            CHeader x ->
                hang (text "CHeader") 2 (pPrint x)
            CTabber x ->
                hang (text "CTabber") 2 (pPrint x)
            CTemplate x ->
                hang (text "CTemplate") 2 (pPrint x)

mkQuoteArchive :: String -> QuoteArchive
mkQuoteArchive raw = case parse (pArchive <* eof) "" raw of
    Right x -> x
    Left _ -> QARaw raw
  where
    validIdCh x =
        isDigit x
        || isAsciiUpper x
        || isAsciiLower x
    pLibId :: Parser LibraryId
    pLibId = some (satisfy validIdCh)
    pSituation :: Parser Int
    pSituation = choice (f <$> kcwikiTable)
      where
        f (sStr, sCode) = sCode <$ string sStr
    pArchive = do
        libId <- pLibId
        _ <- char '-'
        sCode <- pSituation
        extra <- many anyChar
        pure (QANormal libId sCode extra)

data PageType = ShipInfo | Seasonal

type family PageContent a where
    PageContent 'ShipInfo = (TabberRows, [(Header,[QuoteLine])])
    PageContent 'Seasonal = [QuoteLine]

-- key: MasterId, value: (key: situation code, value: QuoteLine)
type ShipQuoteTable = IM.IntMap (IM.IntMap QuoteLine)
