module MiniJson.Types where

import qualified Data.Text as T

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
