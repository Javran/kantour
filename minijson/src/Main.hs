{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Data.Text as T
import Data.Attoparsec.Text
import Control.Applicative
import Data.Char
import Data.Functor
import Data.Void
import Control.Monad

-- instead of what Data.Aeson does,
-- we actually want to work on top of object key order preserving data structure
-- which motivates this small project.

data JValue
  = JText T.Text
  | JNum Void -- just to be a reminder that "JNum" is yet to be implemented
  | JObject [(T.Text, JValue)]
  | JArray [JValue]
  | JBool Bool
  | JNull

pValue :: Parser JValue
pValue = skipSpace >> do
    ahead <- peekChar'
    case ahead of
        '"' -> pStr
        '{' -> pObj
        '[' -> pArr
        't' -> string "true" >> pure (JBool True)
        'f' -> string "false" >> pure (JBool False)
        'n' -> string "null" >> pure JNull
        '-' -> pNum
        _ | isDigit ahead -> pNum
        _ -> fail $ "unexpected leading character: " ++ [ahead]
  where
    -- all following parsers assume a non-space at beginning
    pStr, pObj, pNum, pArr :: Parser JValue

    pPair :: Parser (T.Text, JValue)
    pPair = do
        (JText k) <- pStr
        skipSpace >> void (char ':')
        v <- pValue
        pure (k,v)
    pObj =
        char '{' >>
        JObject <$> pPair `sepBy` (skipSpace >> char ',') <*
        char '}'
    pArr =
        char '[' >>
        JArray <$> pValue `sepBy` (skipSpace >> char ',') <*
        char ']'

    pStr =
        char '"' >>
        (JText . T.pack) <$> many pChar <*
        char '"'
      where
        toChr :: String -> Char
        toChr xs = chr $ sum $ zipWith (*) (map digitToInt $ reverse xs) hexs
          where
            hexs = 1 : map (* 16) hexs
        pChar :: Parser Char
        pChar = do
            ahead <- peekChar'
            case ahead of
                '"' -> mzero
                '\\' ->
                    char '\\' >>
                    (   (char '"' <* pure '"')
                    <|> (char '\\' <* pure '\\')
                    <|> (char '/' <* pure '/')
                    <|> (char 'b' <* pure '\b')
                    <|> (char 'f' <* pure '\f')
                    <|> (char 'n' <* pure '\n')
                    <|> (char 'r' <* pure '\r')
                    <|> (char 't' <* pure '\t')
                    <|> (char 'u' >> toChr <$> replicateM 4 (satisfy isHexDigit))
                    )
                _ | isControl ahead -> mzero
                _ -> pure ahead
    pNum = undefined

main :: IO ()
main = pure ()
