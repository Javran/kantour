{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Data.Text as T
import Data.Attoparsec.Text
import Data.Char
import Data.Functor

-- instead of what Data.Aeson does,
-- we actually want to work on top of object key order preserving data structure
-- which motivates this small project.

data JValue
  = JText T.Text
  | JNum
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
    pNum = undefined
    pStr = undefined

main :: IO ()
main = pure ()
