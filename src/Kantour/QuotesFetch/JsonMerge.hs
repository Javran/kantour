module Kantour.QuotesFetch.JsonMerge where

import Data.Aeson
import qualified Data.Vector as Vec
import qualified Data.HashMap.Lazy as HM

-- left-biased JSON merge
mergeJSON :: Value -> Value -> Value
mergeJSON (Array arr1) (Array arr2) =
    let zipped = Vec.zipWith mergeJSON arr1 arr2
        lz = Vec.length zipped
        ar1' = Vec.drop lz arr1
        ar2' = Vec.drop lz arr2
    in Array (zipped Vec.++ ar1' Vec.++ ar2')
mergeJSON (Object obj1) (Object obj2) =
    Object (HM.unionWith mergeJSON obj1 obj2)
mergeJSON v1 _ = v1

