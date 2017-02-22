module Types where

import Language.Lua

type Quotes = [(String,String)]
type QuotesSection = (String, [Quotes])

-- for type-level tagging
newtype ShipDatabase = ShipDb [TableField]

{-

  (String, [QuotesSection])

- String for link name

- QuotesSection = (String, [Quotes])

    - String for header name
    - [Quotes] a list of quotes

    - a single Quotes consists of some key-value bindings

-}
