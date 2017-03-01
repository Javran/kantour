{-# LANGUAGE QuasiQuotes #-}
module Kantour.QuotesFetch.QParserSpec where

import Test.Hspec
import Test.Hspec.Megaparsec
import Text.Megaparsec
import Kantour.QuotesFetch.QParser
import Text.Heredoc

{-# ANN module "HLint: ignore Redundant do" #-}

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "pHeader" $ do
        let parse' = parse pHeader ""
        specify "header lvl 1" $
            parse' "=h1=" `shouldParse` Header 1 "h1"
        specify "header lvl 2" $
            parse' "==h2==" `shouldParse` Header 2 "h2"
        specify "header lvl 3" $
            parse' "===h3===" `shouldParse` Header 3 "h3"
        specify "fail on empty input" $
            parse' `shouldFailOn` ""
    describe "pElemAsText" $ do
        let parse' = parse pElemAsText ""
        specify "fail on empty input" $
            parse' `shouldFailOn` ""
        specify "parsing spaces" $ do
            parse' "   " `shouldParse` " "
            parse' "\t\n\t" `shouldParse` " "
            parse' "\n\n" `shouldParse` "\n"
        specify "parsing <br> tag" $ do
            parse' "<br>" `shouldParse` "\n"
            parse' "<br/>" `shouldParse` "\n"
            parse' "<br  />" `shouldParse` "\n"
        specify "parsing <ref> tag" $ do
            parse' "<ref></ref>" `shouldParse` ""
            parse' "<ref>content ignored</ref>" `shouldParse` ""
        specify "parsing mediawiki links" $ do
            parse' "[[Link!]]" `shouldParse` "Link!"
            parse' "[[foo   |only this part]]" `shouldParse` "only this part"
            parse' "[foo only this part]" `shouldParse` "only this part"
        specify "parsing templates" $ do
            parse' "{{ignored}}" `shouldParse` ""
            parse' "{{台词翻译表|a=b|c=d}}" `shouldParse` ""
            parse' "{{lang|zh-cn|大白兔是人类的好朋友}}"
                `shouldParse` "大白兔是人类的好朋友"
            parse' "{{lang|zh-cn|[[@upsuper|大白兔]]是人类的好朋友}}"
                `shouldParse` "大白兔是人类的好朋友"
    describe "pTemplate" $ do
        let parse' = parse pTemplate ""
        specify "parsing quote list begin" $ do
            parse' "{{台词翻译表/页头}}"
                `shouldParse` TplQuoteListBegin []
            parse' "{{台词翻译表/页头|type=seasonal}}"
                `shouldParse` TplQuoteListBegin [(Just "type", "seasonal")]
        specify "parsing list end" $
            parse' "{{页尾}}"
                `shouldParse` TplEnd []
        specify "parsing lang" $
            parse' "{{lang|foo|bar}}"
                `shouldParse` TplLang [(Nothing,"foo"),(Nothing,"bar")]
        specify "parsing quotes" $ do
            parse'
                [str|{{台词翻译表|type=seasonal
                    | | 档名 = 184-Sec1Valentine2017
                    | | 编号 = 184
                    | | 舰娘名字 = 大鲸
                    | | 日文台词 = {{lang|jp|て・い・と・く♪　はい！　[[大鯨]]からのチョコレート、どうか受け取って下さい。あ、ありがとうございます♪}}
                    | | 中文译文 = T·I·D·U♪来！请务必收下，这份大鲸的巧克力。谢，谢谢♪
                    |}}
                    |]
               `shouldParse` TplQuote
                   [ (Just "type","seasonal")
                   , (Just "档名","184-Sec1Valentine2017")
                   , (Just "编号", "184")
                   , (Just "舰娘名字","大鲸")
                     -- spaces are normalized so the text does look a bit different, but that shouldn't matter
                   , (Just "日文台词", "て・い・と・く♪ はい！ 大鯨からのチョコレート、どうか受け取って下さい。あ、ありがとうございます♪")
                   , (Just "中文译文", "T·I·D·U♪来！请务必收下，这份大鲸的巧克力。谢，谢谢♪")
                   ]
    describe "pTabber" $ do
        let parse' = parse pTabber ""
        specify "parsing normal tabber" $
            parse' [str|<tabber>
                       |大鲸={{舰娘资料|编号=184}}
                       ||-|
                       |龙凤={{舰娘资料|编号=185}}
                       ||-|
                       |龙凤改={{舰娘资料|编号=190}}
                       |</tabber>
                       |]
            `shouldParse` [("大鲸","184"),("龙凤","185"),("龙凤改","190")]
        specify "parsing tabber with extras" $
            parse' [str|<tabber>
                       |丸输={{舰娘资料|编号=163|运提供=1}}
                       ||-|
                       |丸输改={{舰娘资料|编号=163a|运提供=1|婚后耐久=11}}
                       |</tabber>
                       |]
            `shouldParse` [("丸输","163"),("丸输改","163a")]
