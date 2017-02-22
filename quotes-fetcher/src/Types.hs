module Types where

type Quotes = [(String,String)]
type QuotesSection = (String, [Quotes])

{-

  (String, [QuotesSection])

- String for link name

- QuotesSection = (String, [Quotes])

    - String for header name
    - [Quotes] a list of quotes

    - a single Quotes consists of some key-value bindings

-}
