module Kantour.QuotesFetch.Types where

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
type RawQSection = (SectionName, [RawQuote])
type RawPage = ([TabberRow], [RawQSection])

type MasterId = Int
type LibraryId = String -- library id is used by kcwiki
