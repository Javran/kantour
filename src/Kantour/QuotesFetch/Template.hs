{-|
Module: Kantour.QuotesFetch.Template
Description: Kcwiki templates

Templates supported by kcwiki.
Note that the representation we use is by no means complete
but is extended sufficient to serve the purpose.
|-}
{-# LANGUAGE TupleSections #-}
module Kantour.QuotesFetch.Template
  ( Template
  , tIsSeasonal
  , tLang, tContent
  , tLibId
  , tName, tArgs

  , TemplateArg
  , fromRawTemplate
  ) where

import Kantour.QuotesFetch.Types
import Data.Maybe
import Data.Typeable
import Control.Monad

data Template
  = TplQuote
    { tArgs :: [TemplateArg] }
  | TplQuoteListBegin
    -- ^ <https://zh.kcwiki.moe/wiki/Template:%E5%8F%B0%E8%AF%8D%E7%BF%BB%E8%AF%91%E8%A1%A8/%E9%A1%B5%E5%A4%B4>
    { tIsSeasonal :: Bool
    }
  | TplEnd
    -- ^ <https://zh.kcwiki.moe/wiki/Template:%E9%A1%B5%E5%B0%BE>
  | TplLang
    -- ^ <https://zh.kcwiki.moe/wiki/Template:Lang>
    { tLang :: Maybe String
    , tContent :: Maybe String
    }
  | TplShipInfo
    -- ^ <https://zh.kcwiki.moe/wiki/Template:%E8%88%B0%E5%A8%98%E8%B5%84%E6%96%99>
    { tLibId :: Maybe LibraryId
    }
  | TplUnknown
    -- ^ general representation of not yet recognized templates
    { tName :: String
    , tArgs :: [TemplateArg]
    }
  deriving (Eq, Show, Typeable)

type TemplateArg =
  ( Maybe String -- optional key
  , String -- value
  )

{-|
  ignore 'Nothing's and convert rest of a template
  argument pairs into a table for further looking up
-}
tArgsToTable :: [TemplateArg] -> [(String,String)]
tArgsToTable = concatMap (\(mk,v) -> maybeToList ((,v) <$> mk))

fromRawTemplate :: String -> [TemplateArg] -> Template
fromRawTemplate tpName tpArgs = case tpName of
    "台词翻译表/页头" ->
        TplQuoteListBegin (lkup "type" == Just "seasonal")
    "台词翻译表" -> TplQuote tpArgs
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
