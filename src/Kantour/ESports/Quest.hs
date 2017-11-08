module Kantour.ESports.Quest where

import Data.Coerce

newtype QuestCode = QuestCode String

data Quest = Quest
  { code :: QuestCode
  , desc :: String
    -- number of times needed
  , count :: Int
  , requires :: [QuestCode]
  }

allQuests :: [Quest]
allQuests =
    [ mkQuest "Bd1" "敵艦隊を撃破せよ！" 1 []
    , mkQuest "Bd2" "敵艦隊主力を撃滅せよ！" 1 $ w "Bd1"
    , mkQuest "Bd3" "敵艦隊を10回邀撃せよ！" 10 $ w "Bd2"
    , mkQuest "Bd4" "敵空母を3隻撃沈せよ！" 3 $ w "Bd1"
    , mkQuest "Bd5" "敵補給艦を3隻撃沈せよ！" 3 $ w "Bd2"
    , mkQuest "Bd6" "敵輸送船団を叩け！" 5 $ w "Bd1"
    , mkQuest "Bd7" "南西諸島海域の制海権を握れ！" 5 $ w "Bd5"
    , mkQuest "Bd8" "敵潜水艦を制圧せよ！" 6 $ w "Bd7"

    , mkQuest "Bw1/1" "あ号作戦/出撃36回" 36 $ w "Bd2"
    , mkQuest "Bw1/2" "あ号作戦/S勝利6回" 6 $ w "Bd2"
    , mkQuest "Bw1/3" "あ号作戦/ボス到達24回" 24 $ w "Bd2"
    , mkQuest "Bw1/4" "あ号作戦/ボス戦勝利12回" 12 $ w "Bd2"
    , mkQuest "Bw1" "あ号作戦" 0 $ w "Bw1/1 Bw1/2 Bw1/3 Bw1/4"
    , mkQuest "Bw2" "い号作戦" 20 $ w "Bd5"
    , mkQuest "Bw3" "海上通商破壊作戦" 20 $ w "Bd2"
    , mkQuest "Bw4" "ろ号作戦" 50 $ w "Bw1"
    , mkQuest "Bw5" "海上護衛戦" 15 $ w "Bw2"
    , mkQuest "Bw6" "敵東方艦隊を撃滅せよ！" 12 $ w "Bw5"
    , mkQuest "Bw7" "敵北方艦隊主力を撃滅せよ！" 5 $ w "Bw5"
    , mkQuest "Bw8" "敵東方中枢艦隊を撃破せよ！" 1 $ w "Bw6"
    , mkQuest "Bw9" "南方海域珊瑚諸島沖の制空権を握れ！" 2 $ w "Bw8"
    , mkQuest "Bw10" "海上輸送路の安全確保に努めよ！" 3 $ w "Bw4"

    , mkQuest "Bm1" "「第五戦隊」出撃せよ！" 1 []
    , mkQuest "Bm2" "「潜水艦隊」出撃せよ！" 3 []
    , mkQuest "Bm3" "「水雷戦隊」南西へ！" 1 $ w "Bw4"
    , mkQuest "Bm4" "「水上打撃部隊」南方へ！" 1 []
    , mkQuest "Bm5" "海上護衛強化月間" 10 $ w "Bm1"
    , mkQuest "Bm6" "「空母機動部隊」西へ！" 1 $ w "Bw4"
    , mkQuest "Bm7" "「水上反撃部隊」突入せよ！" 1 $ w "Bm6"

    , mkQuest "Bq1" "沖ノ島海域迎撃戦" 2 $ w "Bm6"
    , mkQuest "Bq2/1" "戦果拡張任務！「Z作戦」前段作戦/2-4" 1 $ w "Bw2"
    , mkQuest "Bq2/2" "戦果拡張任務！「Z作戦」前段作戦/6-1" 1 $ w "Bw2"
    , mkQuest "Bq2/3" "戦果拡張任務！「Z作戦」前段作戦/6-3" 1 $ w "Bw2"
    , mkQuest "Bq2/4" "戦果拡張任務！「Z作戦」前段作戦/6-4" 1 $ w "Bw2"
    , mkQuest "Bq2" "戦果拡張任務！「Z作戦」前段作戦" 0 $ w "Bq2/1 Bq2/2 Bq2/3 Bq2/4"
    , mkQuest "Bq3" "強行輸送艦隊、抜錨！" 2 []
    , mkQuest "Bq4" "前線の航空偵察を実施せよ！" 2 $ w "Bq3"

    , mkQuest "EO1-5" "Extra Opeartion 1-5" 4 []
    , mkQuest "EO1-6" "Extra Opeartion 1-6" 7 $ w "EO1-5"
    , mkQuest "EO2-5" "Extra Opeartion 2-5" 4 []
    , mkQuest "EO3-5" "Extra Operation 3-5" 4 []
    , mkQuest "EO4-5" "Extra Operation 4-5" 5 []
    , mkQuest "EO5-5" "Extra Operation 5-5" 5 []
    , mkQuest "EO6-5" "Extra Operation 6-5" 6 []
    ]
  where
    w = words
    mkQuest code' desc' cnt dep =
        Quest (QuestCode code') desc' cnt (coerce (dep :: [String]))
