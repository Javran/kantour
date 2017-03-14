module Kantour.MiniJson.Types where

import qualified Data.Text as T
import Text.PrettyPrint
import Text.PrettyPrint.HughesPJClass
import Data.Char

-- INVARIANT: all appearance of Integer below should all be non-negative
data JValue
  = JText T.Text
  | JNum
    { jnNeg :: Bool
    , jnBeforeDot :: Integer
    , jnAfterDot :: Maybe Integer
    , jnEs :: Maybe (Bool {- neg sign -}, Integer)
    }
  | JObject [(T.Text, JValue)]
  | JArray [JValue]
  | JBool Bool
  | JNull
    deriving (Show)

instance Pretty JValue where
    pPrint JNull = text "null"
    pPrint (JBool b) = text $ if b then "true" else "false"
    pPrint (JText t) = -- TODO: handle escaping?
        let escaped :: Char -> Doc
            escaped '\\' = text "\\\\"
            escaped '"' = text "\\\""
            escaped c
                | isControl c = error "control chars are not yet handled"
                | otherwise = text [c]
        in char '"' <> foldMap escaped (T.unpack t) <> char '"'
    pPrint (JArray xs) = brackets . fsep . punctuate comma . map pPrint $ xs
    pPrint (JNum n bd mad mes) = pNeg <> pBefore <> pAfter <> pEs
      where
        pNeg = if n then char '-' else empty
        pBefore = integer bd
        pAfter = maybe empty (\ad -> char '.' <> integer ad) mad
        pEs = maybe empty (\(eNeg,ePart) ->
                           let pN' = if eNeg then char '-' else empty
                           in text "e" <> pN' <> integer ePart) mes
    pPrint (JObject xs) = brackets . fsep . punctuate comma . map pPair $ xs
      where
        pPair :: (T.Text, JValue) -> Doc
        pPair (k,v) = parens (text (T.unpack k) <> comma <> pPrint v)
