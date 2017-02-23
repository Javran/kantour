{-# LANGUAGE OverloadedStrings #-}
module Kantour.MiniJson.Parser where

import Prelude hiding (takeWhile)
import qualified Data.Text as T
import Data.Attoparsec.Text
import Control.Applicative
import Data.Char
import Data.Functor
import Control.Monad

import Kantour.MiniJson.Types

-- all following parsers assume a non-space at beginning
pPair :: Parser (T.Text, JValue)
pPair = do
    (JText k) <- pStr
    skipSpace >> void (char ':') >> skipSpace
    v <- pValue
    pure (k,v)

pValue :: Parser JValue
pValue = do
    ahead <- peekChar'
    case ahead of
        '"' -> pStr
        '{' -> pObj
        '[' -> pArr
        't' -> JBool True <$ "true"
        'f' -> JBool False <$ "false"
        'n' -> JNull <$ "null"
        '-' -> pNum
        _ | isDigit ahead -> pNum
        _ -> fail $ "unexpected leading character: " ++ [ahead]

pStr, pObj, pNum, pArr :: Parser JValue

pObj =
        char '{' >>
        (JObject <$> pPair `sepBy` (skipSpace >> char ',' >> skipSpace)) <*
        char '}'
pArr =
        char '[' >>
        (JArray <$> pValue `sepBy` (skipSpace >> char ',' >> skipSpace)) <*
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
                    (   ('"' <$ char '"')
                    <|> ('\\' <$ char '\\')
                    <|> (char '/' <* pure '/')
                    <|> (char 'b' <* pure '\b')
                    <|> (char 'f' <* pure '\f')
                    <|> (char 'n' <* pure '\n')
                    <|> (char 'r' <* pure '\r')
                    <|> (char 't' <* pure '\t')
                    <|> (char 'u' >> toChr <$> replicateM 4 (satisfy isHexDigit))
                    )
                _ | isControl ahead -> mzero
                _ -> anyChar
pNum = do
        -- sign
        sign <- option False (True <$ char '-')
        -- before dot
        let isDigit1to9 x = x /= '0' && isDigit x
        beforeDot <- ("0" >> pure 0)
                 <|> (do
                         -- look ahead just to make sure the first digit
                         -- is 1~9
                         ahead <- peekChar'
                         guard (isDigit1to9 ahead)
                         decimal
                     )
        -- after dot
        afterDot <- option Nothing (char '.' >> Just <$> decimal)
        -- exp part
        ep <- option Nothing $ do
            void $ char 'e' <|> char 'E'
            signE <- option False ((False <$ char '+')
                               <|> (True <$ char '-'))
            ds <- decimal
            pure (Just (signE, ds))
        pure (JNum sign beforeDot afterDot ep)
