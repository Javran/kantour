module Kantour.QuotesFetch.QParser where

import Data.List

import Text.Megaparsec
import Text.Megaparsec.String
import Data.Char

import Kantour.QuotesFetch.Types
{-
this module aims at providing a more sophisticated quote parsing solution
than just simply using ReadP.

we are not exactly parsing the whole MediaWiki format, but parsing
just a small subset of it that has enough quote-related info that we want.
-}

data Template
  = TplQuote
  { tArgs :: [TemplateArg] }
  | TplQuoteListBegin
  { tArgs :: [TemplateArg] }
  | TplEnd
  { tArgs :: [TemplateArg] }
  | TplLang
  { tArgs :: [TemplateArg] }
  | TplUnknown
  { tName :: String
  , tArgs :: [TemplateArg] }
  deriving (Eq, Show)

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
pTemplate =
    between
      (string "{{")
      (string "}}")
      $ do
          -- PCCB: pipe, curly closing bracket
          -- PCCBE: PCCB + equal
          let notPCCB x = x /= '|' && x /= '}'
              notPCCBE x = x /= '=' && notPCCB x
              -- normalize key / value by cutting first and last whitespace
              -- we only need to cut one space out because "pElemAsText" should
              -- have collapsed consecutive whitespaces into one
              normKV [] = []
              normKV inp@(x:xs) =
                  if isSpace (last ys)
                    then init ys
                    else ys
                where
                  ys = if isSpace x
                         then xs
                         else inp
          tpName <- some (satisfy notPCCB)
          let pArg = do
                  raw1 <- concat <$>
                            some (pElemAsText
                                  <|> pure <$> satisfy notPCCBE)
                  raw2 <- option
                            Nothing
                            (Just . concat <$> (char '=' >>
                                       some (pElemAsText
                                             <|> pure <$> satisfy notPCCB)))
                  case raw2 of
                      Nothing -> pure (Nothing, normKV raw1)
                      Just raw2' -> pure (Just (normKV raw1), normKV raw2')
          tpArgs <- option
                      []
                      $ char '|' >> pArg `sepBy` (char '|' >> space)
          pure $ case tpName of
              "台词翻译表/页头" -> TplQuoteListBegin tpArgs
              "台词翻译表" -> TplQuote tpArgs
              "页尾" -> TplEnd tpArgs
              "lang" -> TplLang tpArgs
              _ -> TplUnknown tpName tpArgs

tplAsText :: Template -> String
-- {{lang|<language>|<content>}}
tplAsText (TplLang [_,(Nothing,content)]) = content
tplAsText _ = ""

{-
TODO: do we get a speed boost, if dlist, rather than [Char] is used?

parsing some special elements as text
-}
pElemAsText :: Parser String
pElemAsText =
    pSpaces
    <|> pBr
    <|> pRef
    <|> pLink
    <|> pTemplateAsText
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
        (endP,sepChar) <- ((string "]]",'|') <$ string "[[")
                          <|> ((string "]",' ') <$ string "[")
        linkContent <- manyTill anyChar endP
        let content1 = dropWhile (/= sepChar) linkContent
        case content1 of
            [] ->
                -- pipe not found, return full content
                pure linkContent
            sep:content2 | sep == sepChar ->
                -- cut first part and return the rest of it
                pure content2
            _ -> error "pText: pLink: unreachable"
    pTemplateAsText :: Parser String
    pTemplateAsText = tplAsText <$> pTemplate

pTabber :: Parser [TabberRow]
pTabber = between
    (string "<tabber>")
    (string "</tabber>")
    $ do
        let item = do
                tmName <- some (satisfy (/= '='))
                _ <- char '=' <* space
                tTpl <- pTemplate
                case tTpl of
                    TplUnknown "舰娘资料" pairs
                        | Just nStr <- lookup "编号"
                                       $ concatMap (\(mk,v) -> maybe [] (\k -> [(k,v)]) mk)
                                         pairs
                          -> pure (tmName, nStr)
                    _ -> fail "pTabber: unexpected template"
        space
        (item <* space) `sepBy1` (string "|-|" <* space)
