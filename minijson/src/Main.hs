{-# LANGUAGE OverloadedStrings #-}
module Main where

import Prelude hiding (takeWhile)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Attoparsec.Text
import Control.Applicative
import Data.Foldable
import Data.Char
import Data.Functor
import Control.Monad
import System.Environment

-- instead of what Data.Aeson does,
-- we actually want to work on top of object key order preserving data structure
-- which motivates this small project.

data JValue
  = JText T.Text
  | JNum
    { jnNeg :: Bool
    , jnBeforeDot :: [Int]
    , jnAfterDot :: [Int]
    , jnEs :: Maybe (Bool {- neg sign -}, [Int])
    }
  | JObject [(T.Text, JValue)]
  | JArray [JValue]
  | JBool Bool
  | JNull
    deriving (Show)

-- list of digits to integer, preserving empty list by wraping around Maybe
digitsToInt :: [Int] -> Maybe Integer
digitsToInt [] = Nothing
digitsToInt xs = Just (foldl' (\acc i -> acc * 10 + fromIntegral i) 0 xs)

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
        skipSpace >> void (char ':') >> skipSpace
        v <- pValue
        pure (k,v)
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
                _ -> anyChar
    pNum = do
        let convert = map digitToInt . T.unpack
        -- sign
        sign <- option False (char '-' >> pure True)
        -- before dot
        let isDigit1to9 x = x /= '0' && isDigit x
        beforeDot <- (string "0" >> pure [0])
                 <|> (do
                         c <- satisfy isDigit1to9
                         cs <- T.unpack <$> takeWhile1 isDigit
                         pure (map digitToInt (c : cs))
                     )
        -- after dot
        afterDot <- option [] (char '.' >> convert <$> takeWhile1 isDigit)
        -- exp part
        ep <- option Nothing $ do
            void $ char 'e' <|> char 'E'
            signE <- option False ((char '+' >> pure False)
                               <|> (char '-' >> pure True))
            ds <- takeWhile1 isDigit
            pure (Just (signE, convert ds))
        pure (JNum sign beforeDot afterDot ep)

main :: IO ()
main = do
    [srcFP] <- getArgs
    content <- T.readFile srcFP
    let parsed = parseOnly pValue content
    print parsed
