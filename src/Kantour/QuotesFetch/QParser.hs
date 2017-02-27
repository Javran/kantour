module Kantour.QuotesFetch.QParser where

import Text.Megaparsec
import Text.Megaparsec.String

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

this time we don't over do it: every parser assumes beginning at a non-space
character, but just consume what it is supposed to consume, *not* including
ending spaces. by doing so we can make good use of the "newline" hint.
that is: things like headers and templates can only appear right at the beginning of a line.

I think it's very promising that we don't need any backtracking at all.

-}

data Header = Header
  { hdLevel :: Int
  , hdContent :: String
  }

pHeader :: Parser Header
pHeader = do
    eqSigns <- some (char '=')
    content <- some (satisfy (/= '='))
    _ <- string eqSigns
    pure (Header (length eqSigns) content)

pTemplate :: Parser Template
pTemplate = do
    _ <- string "{{"
    tpName <- some (noneOf "|}")
    let pArg = do
            raw1 <- some (noneOf "=|}")
            raw2 <- option
                      Nothing
                      (Just <$> (char '=' >> some (noneOf "|}")))
            case raw2 of
                Nothing -> pure (Nothing, raw1)
                Just raw2' -> pure (Just raw1, raw2')
    tpArgs <- pArg `sepBy` (char '|' >> space) :: Parser [TemplateArg]
    _ <- string "}}"
    pure (Template tpName tpArgs)
