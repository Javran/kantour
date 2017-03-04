{-# LANGUAGE TupleSections #-}
module Kantour.QuotesFetch.QParser where

import Data.List

import Text.Megaparsec
import Text.Megaparsec.String
import Data.Maybe
import Data.Functor
import Kantour.QuotesFetch.Types
import Kantour.QuotesFetch.Kcwiki

import Kantour.Utils
import Control.Monad
{-
this module aims at providing a more sophisticated quote parsing solution
than just simply using ReadP.

we are not exactly parsing the whole MediaWiki format, but parsing
just a small subset of it that has enough quote-related info that we want.
-}

{-

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

-- ignore "Nothing" and convert rest of a template
-- arg pairs into a table for further looking up
tArgsToTable :: [TemplateArg] -> [(String,String)]
tArgsToTable = concatMap (\(mk,v) -> maybeToList ((,v) <$> mk))

pHeader :: Parser Header
pHeader = do
    eqSigns <- some (char '=')
    content <- some (satisfy (/= '='))
    _ <- string eqSigns
    pure (Header (length eqSigns) (strip content))

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
          tpName <- strip <$> some (satisfy notPCCB)
          let pArg = do
                  -- raw1 could be key or value, depending on whether
                  -- there is a following "=" sign
                  raw1 <- concat <$>
                            many (pElemAsText
                                  <|> pure <$> satisfy notPCCBE)
                  raw2 <- option
                            Nothing
                            (Just . concat <$> (char '=' >>
                                       many (pElemAsText
                                             <|> pure <$> satisfy notPCCB)))
                  case raw2 of
                      Nothing ->
                          pure (Nothing, strip raw1)
                      Just raw2' -> do
                          let k = strip raw1
                          when (null k)
                            (fail "pTemplate: expecting non-empty key")
                          pure (Just k, strip raw2')
          tpArgs <- option
                      []
                      $ char '|' >> pArg `sepBy` (char '|' >> space)
          pure $ fromRawTemplate tpName tpArgs

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
    pTemplateAsText = templateAsText <$> pTemplate

pTabber :: Parser TabberRows
pTabber = between
    (string "<tabber>")
    (string "</tabber>")
    $ do
        let item = do
                tmName <- some (satisfy (/= '='))
                _ <- char '=' <* space
                tTpl <- pTemplate
                case tLibId tTpl of
                    Just nStr -> pure (tmName, nStr)
                    _ -> fail "pTabber: unexpected template"
        space
        TR <$> (item <* space) `sepBy1` (string "|-|" <* space)

-- scan and parse a document line-by-line
-- and only retrieve things recognizable
-- as we know headers and templates always begins
-- without any padding, this should be a perfect method
-- to reduce the amount of backtracking.
pScanAll :: Parser Page
pScanAll = Page . catMaybes <$> manyTill pScan eof
  where
    pScan :: Parser (Maybe Component)
    pScan =
            (Just . CHeader <$> pHeader <* untilEol)
        <|> (Just . CTabber <$> pTabber <* untilEol)
        <|> (Just . CTemplate <$> pTemplate <* untilEol)
        <|> Nothing <$ untilEol
    untilEol = manyTill anyChar (void eol <|> eof)

pCollectLinks :: Parser [String]
pCollectLinks =
    catMaybes <$> many ((checkLink <$> pLink)
                        <|> (Nothing <$ anyChar)) <* eof
  where
    pLink :: Parser String
    pLink = between
        (string "[[")
        (string "]]")
        $ do
            raw1 <- some (satisfy (\x -> x /= '|' && x /= ']'))
            _ <- option Nothing (Just <$> (char '|' >> some (satisfy (/= ']'))))
            pure raw1

    checkLink :: String -> Maybe String
    checkLink x = do
        guard (not (nonKanmusuLink x))
        guard (x `notElem` knownShipTypeNames)
        pure x

    nonKanmusuLink :: String -> Bool
    nonKanmusuLink xs =
           "File:" `isPrefixOf` xs
        || "template:" `isPrefixOf` xs
        || "分类:" `isPrefixOf` xs
    knownShipTypeNames :: [String]
    knownShipTypeNames =
        [ "驅逐艦"
        , "輕巡洋艦"
        , "重雷装巡洋舰"
        , "重巡洋艦"
        , "航空巡洋艦"
        , "戰艦"
        , "航空戰艦"
        , "輕空母"
        , "正規空母"
        , "裝甲空母"
        , "潛水艇"
        , "水上飞机母舰"
        , "戰列艦"
        , "航空戰列艦"
        , "重巡洋艦"
        , "航空巡洋艦"
        , "正規空母"
        , "輕空母"
        , "輕巡洋艦"
        , "驅逐艦"
        , "潛水艇"
        ]
