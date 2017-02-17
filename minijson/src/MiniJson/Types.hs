module MiniJson.Types where

import qualified Data.Text as T

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
