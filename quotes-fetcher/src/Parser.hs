module Parser where

import Text.ParserCombinators.ReadP
import Data.Functor
import Data.List
import Data.Char

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

type Quotes = [(String,String)]
type QuotesSection = (String, [Quotes])

pQuotesSection :: ReadP QuotesSection
pQuotesSection = (,) <$> pHeader3 <*> pQuotesList

pHeader3 :: ReadP String
pHeader3 = token (string "===") *> munch1 (/= '=') <* token (string "===")

pQuotesStr :: ReadP Quotes
pQuotesStr =
    token (string "{{台词翻译表") >> token (string "|")
    >> (pPair `sepBy1` token (char '|'))
    <* token (string "}}")
  where
    pPair = do
        key <- normString <$> munch1 (/= '=')
        _ <- token (char '=')
        val <- normString <$> munch (\c -> c /='|' && c /= '}')
        pure (key,val)
    normString = normString2 . normString1
    normString1 [] = []
    normString1 (x:xs)
        | isSpace x = ' ' : normString1 (dropWhile isSpace xs)
        | otherwise = x : normString1 xs
    normString2 [] = []
    normString2 xs = xs2
      where
        xs1 = if isSpace (head xs)
                then tail xs
                else xs
        xs2 = if isSpace (last xs1)
                then init xs1
                else xs1

pQuotesList :: ReadP [ Quotes ]
pQuotesList =
    token (string "{{台词翻译表/页头}}")
    *> many pQuotesStr <* token (string "{{页尾}}")
