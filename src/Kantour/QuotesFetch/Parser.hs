module Kantour.QuotesFetch.Parser where

import Text.ParserCombinators.ReadP
import Data.Functor
import Data.List
import Data.Char
import Control.Monad
import Data.Monoid
import Kantour.QuotesFetch.Types
import Data.Coerce
{-# ANN module "HLint: ignore Use fromMaybe" #-}

-- quick and dirty parser that just work

{-
transforms a parser so that in addition to what
the parser suppose to parse, it also consumes all trailing spaces
(until hitting next non-space one)
-}
token :: ReadP a -> ReadP a
token = (<* skipSpaces)

pLinks :: ReadP [String]
pLinks = do
    _ <- munch (/='[')
    aheads <- look
    case aheads of
        [] -> pure []
        '[':'[':_ -> do
            ln <- pLink
            lns <- pLinks
            pure (ln:lns)
        _ -> get >> pLinks
  where
    pLink :: ReadP String
    pLink = do
        _ <- string "[["
        content <- munch1 (\c -> c /= '|' && c /=']')
        aheads <- look
        case aheads of
            '|':_ -> void (get >> munch1 (/=']'))
            _ -> pure ()
        _ <- string "]]"
        pure content

extractLinks :: String -> [String]
extractLinks raw
    | [(lns,[])] <- readP_to_S pLinks raw = lns
    | otherwise = []

notKanmusuLink :: String -> Bool
notKanmusuLink xs =
       "File:" `isPrefixOf` xs
    || "template:" `isPrefixOf` xs
    || "分类:" `isPrefixOf` xs

{-

parse the full document, skip non-'=' signs, then either:

1. try parsing one under cursor as quote section, then consume all remaining outputs

or:

2. consume '=' and continue search recursively

the decision is non-deterministic so we might get more than one result
and all of them represents a valid quote section

-}
pFullScan :: ReadP QuotesSection
pFullScan = munch (/='=')
    *> ((pQuotesSection <* munch (const True))
        +++ (munch1 (=='=') *> pFullScan))


pQuotesSection :: ReadP QuotesSection
pQuotesSection = (,) <$> pHeader3 <*> pQuotesList

pHeader3 :: ReadP String
pHeader3 = pHeader 3

pHeader :: Int -> ReadP String
pHeader n = deco *> munch1 (/= '=') <* deco
  where
    deco = token (string (replicate n '='))

collapseWs :: String -> String
collapseWs xs = case xs of
    [] -> []
    (y:ys) -> if isSpace y
                then ' ':collapseWs (dropWhile isSpace ys)
                else y:collapseWs ys

strip, lstrip, rstrip :: String -> String
lstrip = dropWhile isSpace
rstrip = reverse . lstrip . reverse
strip = rstrip . lstrip

removeRefs :: String -> String
removeRefs [] = []
removeRefs xs = case removePrefix "<ref>" xs of
    Nothing -> head xs : removeRefs (tail xs)
    Just ys -> removeRefs (removeEndTag ys)
  where
    removeEndTag [] = []
    removeEndTag as = case removePrefix "</ref>" as of
        Nothing -> removeEndTag (tail as)
        Just bs -> bs

removeBrs :: String -> String
removeBrs [] = []
removeBrs xs = case coerce (mconcat [ removePrefix "<br>" xs
                                    , removePrefix "<br/>" xs
                                    , removePrefix "<br />" xs]) of
    Nothing -> head xs : removeBrs (tail xs)
    Just ys -> removeBrs ys

-- a more informative version of isPrefixOf
removePrefix :: String -> String -> Maybe String
removePrefix xs ys = do
    guard $ xs `isPrefixOf` ys
    pure (drop (length xs) ys)

pQuotesStr :: ReadP Quotes
pQuotesStr =
    token (string "{{台词翻译表") >> token (string "|")
    >> (pPair `sepBy1` token (char '|'))
    <* token (string "}}")
  where
    -- TODO: <br/> ?
    -- TODO: do it a different way:
    -- <ref> and <br> should be dealt with in a separated "Quotes" module
    pPair = do
        key <- normString <$> munch1 (/= '=')
        _ <- token (char '=')
        val <- removeBrs . removeRefs . normString <$> munch (\c -> c /='|' && c /= '}')
        pure (key,val)
    normString = strip . collapseWs

pQuotesList :: ReadP [ Quotes ]
pQuotesList =
    token (string "{{台词翻译表/页头}}")
    *> many pQuotesStr <* token (string "{{页尾}}")

{-
TODO: sample

==舰娘属性==
<tabber>
朝风={{舰娘资料|编号=272}}
|-|
朝风改={{舰娘资料|编号=272a}}
</tabber>

output:

[ (朝风,272)
, (朝风改,272a)
]

-}

type TabberRow = (String, String)

pTabber :: ReadP [TabberRow]
pTabber = header *> body
  where
    header = token (string "==舰娘属性==")
    body = token (string "<tabber>")
        *> (pair `sepBy1` token (string "|-|"))
        <* token (string "</tabber>")
    pair = do
        name <- munch1 (/= '=')
        _ <- string "={{舰娘资料|编号="
        cid <- munch1 (\x -> x /= '}' && x /= '|')
        _ <- token (munch (/= '}') >> string "}}")
        pure (name,cid)

collectAll :: String -> ([TabberRow], [QuotesSection])
collectAll raw = (trs, results)
  where
    [(trs, leftover)] = readP_to_S pTabber raw
    results = map fst . filter (null . snd) $ readP_to_S pFullScan leftover
