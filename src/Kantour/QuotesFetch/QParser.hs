module Kantour.QuotesFetch.QParser where

{-
this module aims at providing a more sophisticated quote parsing solution
than just simply using ReadP.

we are not exactly parsing the whole MediaWiki format, but parsing
just a small subset of it that has enough quote-related info that we want.
-}

data Template = Template
  { tName :: String
  , tArgs :: [TemplateArg]
  }

type TemplateArg =
  ( Maybe String -- optional key
  , String -- value
  )

{-

TODO:

things we might be interested in parsing:

- Links. but this might not be necessary if we use ship database as
  a guide instead

- Headers

- Templates

- Tabbers, which consists of a Header followed by <tabber> structure

-}
