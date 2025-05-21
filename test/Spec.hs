{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Test.Hspec
import Lib -- Assuming your Lib module exports Postcode, convertTownArea, isSameAddress

-- Helper function to create a default Postcode record for testing.
-- We only care about townArea and townAreaKana for convertTownArea tests.
-- Other fields can be dummy values.
defaultPostcode :: Postcode
defaultPostcode = Postcode
    { jis = "01101"
    , postcode5 = "001"
    , postcode = "0010000"
    , prefectureKana = "ﾎｯｶｲﾄﾞｳ"
    , cityKana = "ｻｯﾎﾟﾛｼｷﾀｸ"
    , townAreaKana = "" -- Will be set by test cases
    , prefecture = "北海道"
    , city = "札幌市北区"
    , townArea = "" -- Will be set by test cases
    , isOneTownByMultiPostcode = 0
    , isNeedSmallAreaAddress = 0
    , isChome = 0
    , isMultiTownByOnePostcode = 0
    , updated = 0
    , updateReason = 0
    }

main :: IO ()
main = hspec $ do
    describe "convertTownArea" $ do
        it "handles ignored patterns" $
            let pc = defaultPostcode { townArea = "以下に掲載がない場合", townAreaKana = "ｲｶﾆｹｲｻｲｶﾞﾅｲﾊﾞｱｲ" }
            in convertTownArea pc `shouldBe` [("", "")]

        it "handles city/town/village next address" $
            let pc = defaultPostcode { townArea = "市の次に番地がくる場合", townAreaKana = "ｼﾉﾂｷﾞﾆﾊﾞﾝﾁｶﾞｸﾙﾊﾞｱｲ" }
            in convertTownArea pc `shouldBe` [("", "")]

        it "handles city/town/village all area" $
            let pc = defaultPostcode { townArea = "町一円", townAreaKana = "ﾏﾁｲﾁｴﾝ" }
            in convertTownArea pc `shouldBe` [("", "")]

        it "extracts floor information (zenkaku)" $
            let pc = defaultPostcode { townArea = "（２階）", townAreaKana = "(2ｶｲ)" } -- Kana for regexFloorKana
            in convertTownArea pc `shouldBe` [("　２階", "2ｶｲ")]
        
        it "extracts floor information (hankaku kana for pattern)" $
            let pc = defaultPostcode { townArea = "（地下１階）", townAreaKana = "(B1ｶｲ)" } -- Example, regexFloorKana uses [0-9]
            in convertTownArea pc `shouldBe` [("　地下１階", "B1ｶｲ")]

        it "extracts Jiwari information" $
            let pc = defaultPostcode { townArea = "某町第１地割", townAreaKana = "ﾎﾞｳﾁｮｳﾀﾞｲ1ﾁﾜﾘ" }
            in convertTownArea pc `shouldBe` [("某町", "ﾎﾞｳﾁｮｳ")]

        it "extracts Jiwari information without '第'" $
            let pc = defaultPostcode { townArea = "某町２地割", townAreaKana = "ﾎﾞｳﾁｮｳ2ﾁﾜﾘ" }
            in convertTownArea pc `shouldBe` [("某町", "ﾎﾞｳﾁｮｳ")]

        it "handles simple parentheses with ignored content (zenkaku)" $
            let pc = defaultPostcode { townArea = "（その他）", townAreaKana = "(ｿﾉﾀ)" }
            in convertTownArea pc `shouldBe` [("", "")]

        it "handles simple parentheses with non-ignored content (zenkaku)" $
            -- Assuming "カスタム" is not in regexIgnoreInParentheses
            let pc = defaultPostcode { townArea = "（カスタム）", townAreaKana = "(ｶｽﾀﾑ)" }
            in convertTownArea pc `shouldBe` [("カスタム", "ｶｽﾀﾑ")]

        it "extracts multiple items from parentheses (zenkaku)" $
            let pc = defaultPostcode { townArea = "（ＡＢＣ、ＤＥＦ）", townAreaKana = "(ABC､DEF)" }
            in convertTownArea pc `shouldBe` [("ＡＢＣ", "ABC"), ("ＤＥＦ", "DEF")]
        
        it "extracts multiple items from parentheses including ignored (zenkaku)" $
            let pc = defaultPostcode { townArea = "（ＡＢＣ、その他、ＤＥＦ）", townAreaKana = "(ABC､ｿﾉﾀ､DEF)" }
            in convertTownArea pc `shouldBe` [("ＡＢＣ", "ABC"), ("ＤＥＦ", "DEF")]

        it "handles regular town area" $
            let pc = defaultPostcode { townArea = "普通の町域", townAreaKana = "ﾌﾂｳﾉﾁｮｳｲｷ" }
            in convertTownArea pc `shouldBe` [("普通の町域", "ﾌﾂｳﾉﾁｮｳｲｷ")]

        it "handles town area with numbers that are not floors (zenkaku)" $
            let pc = defaultPostcode { townArea = "駅前１−２−３", townAreaKana = "ｴｷﾏｴ1-2-3" }
            in convertTownArea pc `shouldBe` [("駅前１−２−３", "ｴｷﾏｴ1-2-3")]

        it "handles town area with parentheses that are not floors or special ignores" $
            let pc = defaultPostcode { townArea = "（Ａ地区）", townAreaKana = "(Aﾁｸ)" }
            in convertTownArea pc `shouldBe` [("Ａ地区", "Aﾁｸ")]

    describe "isSameAddress" $ do
        let addr1 = defaultPostcode { townArea = "元町", townAreaKana = "ﾓﾄﾏﾁ" }
        let addr2 = defaultPostcode { townArea = "元町", townAreaKana = "ﾓﾄﾏﾁ" }
        let addr3 = defaultPostcode { townArea = "新町", townAreaKana = "ｼﾝﾏﾁ" } -- Different townArea
        let addr4 = defaultPostcode { townArea = "元町", townAreaKana = "もとまち" } -- Different townAreaKana
        let addr5 = defaultPostcode { city = "別の市", townArea = "元町", townAreaKana = "ﾓﾄﾏﾁ" } -- Different city

        it "reports same for identical addresses" $
            isSameAddress addr1 addr2 `shouldBe` True

        it "reports different for different townArea" $
            isSameAddress addr1 addr3 `shouldBe` False

        it "reports different for different townAreaKana" $
            isSameAddress addr1 addr4 `shouldBe` False
        
        it "reports different for different city" $
            isSameAddress addr1 addr5 `shouldBe` False

        it "reports same if only non-compared fields differ" $
            let addr6 = addr1 { jis = "99999" } -- jis is not in comparison fields
            in isSameAddress addr1 addr6 `shouldBe` True
