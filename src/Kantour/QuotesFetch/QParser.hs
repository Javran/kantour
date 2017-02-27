module Kantour.QuotesFetch.QParser where

import Data.List

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

some component might appear in key-value part of a template:

- begin with "[" => link
- "<br>" / "<br />" / "<br/>"
- "<ref> ... </ref>"
- "{{lang ...}}

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

{-
TODO: do we get a speed boost, if dlist, rather than [Char] is used?

"pText cond" tries to consume as much input as possible, forcing the result to be text
and stops when a requirement is met.

this parser consumes at least one input upon success, and thus
the first char in the input is *not* checked against "cond".
-}
pText :: (Char -> Bool) -> Parser String
pText stopCond = do
    x <- pElem <|> (pure <$> anyChar)
    xs <- many (pElem <|> (pure <$> satisfy (not . stopCond)))
    pure (concat (x : xs))
  where
    -- consumes one or more whitespaces.
    -- returns a "\n" when the input begins with "\n\n"
    -- otherwise a single " " is returned
    pSpaces :: Parser String
    pSpaces = do
        sp <- some spaceChar
        pure (if "\n\n" `isPrefixOf` sp
                then "\n"
                else " ")
    -- "<br>" / "<br/>" / "<br   />"
    pBr :: Parser String
    pBr = "\n" <$
        (string "<br"
         >> space
         >> (string "/>" <|> string ">"))
    pRef :: Parser String
    pRef = "" <$
         (string "<ref>"
          >> manyTill anyChar (string "</ref>"))
    pLink :: Parser String
    pLink = do
        endP <- (string "]]" <$ string "[[")
                <|> (string "]" <$ string "]")
        linkContent <- manyTill anyChar endP
        let content1 = dropWhile (/= '|') linkContent
        case content1 of
            [] -> pure linkContent -- pipe not found, return full content
            '|':content2 -> pure content2 -- cut first part and return the rest of it
            _ -> error "pText: pLink: unreachable"
    -- TODO: for handling {{lang ...}} stuff.
    pTemplateAsText :: Parser String
    pTemplateAsText = undefined

    pElem = pSpaces <|> pBr <|> pRef <|> pLink <|> pTemplateAsText
