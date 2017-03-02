module Kantour.QuotesFetch.Types where

import Text.PrettyPrint.HughesPJClass
import Data.Coerce

{-
a raw quote is a bunch of key-value pairs
-}
type RawQuote = [(String,String)]

type LinkName = String
type SectionName = String

{-
example of tabber rows:

==舰娘属性==
<tabber>
朝风={{舰娘资料|编号=272}}
|-|
朝风改={{舰娘资料|编号=272a}}
</tabber>

represented as a list of TabberRows:

[ (朝风,272)
, (朝风改,272a)
]

-}
type TabberRow = (String, String)
newtype TabberRows = TR [TabberRow] deriving (Eq, Show)
type RawQSection = (SectionName, [RawQuote])
type RawPage = ([TabberRow], [RawQSection])

type MasterId = Int
type LibraryId = String -- library id is used by kcwiki

pprTabberRows :: [TabberRow] -> Doc
pprTabberRows rows =
    hang (text "Tabber") 2 $
      vcat (map (\(k,v) -> text k <+> text "==>" <+> text v) rows)

instance Pretty TabberRows where
    pPrint = coerce pprTabberRows
