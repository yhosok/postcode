{-# LANGUAGE DeriveGeneric #-}

module Lib where

import           Control.Applicative            ( (<$>)
                                                , (<*>)
                                                )
import qualified Data.ByteString               as B
import qualified Data.ByteString.Lazy          as BL
import           Data.Csv
import qualified Data.Vector                   as V
import           Text.Regex.TDFA
import           Text.Regex
import           System.IO
import           Data.List.Split
import           GHC.Generics                   ( Generic )
import           Data.Maybe                     ( fromMaybe
                                                , fromJust
                                                )
-- import qualified Codec.Text.IConv as IConv

data Postcode = Postcode
    { jis :: !String
    , postcode5 :: !String
    , postcode :: !String
    , prefectureKana :: !String
    , cityKana :: !String
    , townAreaKana :: !String
    , prefecture :: !String
    , city :: !String
    , townArea :: !String
    , isOneTownByMultiPostcode :: !Int
    , isNeedSmallAreaAddress :: !Int
    , isChome :: !Int
    , isMultiTownByOnePostcode :: !Int
    , updated :: !Int
    , updateReason :: !Int
    }
    deriving (Generic, Show)

instance FromRecord Postcode
instance ToRecord Postcode
instance ToNamedRecord Postcode

regexIgnore :: String
regexIgnore = "以下に掲載がない場合|(市|町|村)の次に番地がくる場合|(市|町|村)一円"

regexParentheses :: String
regexParentheses = "（(.*)）"

regexParenthesesKana :: String
regexParenthesesKana = "\\((.*)\\)"

regexUnClosedParentheses :: String
regexUnClosedParentheses = "（.*[^）]$"

regexUnClosedParenthesesKana :: String
regexUnClosedParenthesesKana = "\\(.*[^\\)]$"

regexFloor :: String
regexFloor = "（([０-９]+階)）"

regexFloorKana :: String
regexFloorKana = "\\(([0-9]+ｶｲ)\\)"

regexReplace :: String
regexReplace = "^([^０-９第（]+)[第]*[０-９]+地割.*"

regexReplaceKana :: String
regexReplaceKana = "^([^0-9\\(]+)(ﾀﾞｲ)*[0-9]*ﾁﾜﾘ.*"

regexUseInParentheses :: String
regexUseInParentheses = "[０-９]+区"

regexUseInParenthesesKana :: String
regexUseInParenthesesKana = "[0-9]+ｸ"

regexIgnoreInParentheses :: String
regexIgnoreInParentheses =
    "[０-９]|^その他$|^丁目$|^番地$|^地階・階層不明$|[０-９]*地割|成田国際空港内|次のビルを除く"

regexIgnoreInParenthesesKana :: String
regexIgnoreInParenthesesKana = "ｿﾉﾀ|[0-9]*ﾁﾜﾘ|ﾊﾞｯｶｲ"

convert :: FilePath -> FilePath -> IO ()
convert input output = do
    csvData <- BL.readFile input
    -- csvData' <- return $ IConv.convert "CP932" "UTF-8" csvData
    BL.writeFile output BL.empty
    case decode NoHeader csvData of
        Left  err -> putStrLn err
        Right v   -> V.foldM_ (convert' output) (V.head v) v

convert' :: FilePath -> Postcode -> Postcode -> IO Postcode
convert' output last current
    | isUnCloseLast && isUnCloseNext = return $ current
        { townArea     = townArea next
        , townAreaKana = townAreaKana next
        }
    | isUnCloseLast = writeAndReturn next
    | isUnCloseCurrent = return current
    | otherwise = writeAndReturn current
  where
    next = current
        { townArea     = townArea last ++ townArea current
        , townAreaKana =
            townAreaKana last
                ++ (if isUnCloseKana last then townAreaKana current else "")
        }
    isUnCloseLast    = isUnClose last
    isUnCloseCurrent = isUnClose current
    isUnCloseNext    = isUnClose next
    isUnClose p = townArea p =~ regexUnClosedParentheses :: Bool
    isUnCloseKana p = townAreaKana p =~ regexUnClosedParenthesesKana :: Bool
    writeAndReturn p = do
        write p
        return p
    write = BL.appendFile output . encode . postCodeWithTownArea

postCodeWithTownArea :: Postcode -> [Postcode]
postCodeWithTownArea p = case convertTownArea p of
    Just tas ->
        map (\(ta, taKana) -> p { townArea = ta, townAreaKana = taKana }) tas
    Nothing -> [p { townArea = "", townAreaKana = "" }]

convertTownArea :: Postcode -> Maybe [(String, String)]
convertTownArea p
    | ta =~ regexIgnore
    = Nothing
    | ta =~ regexFloor
    = Just [(replace regexFloor ta "　\\1", replace regexFloorKana taKana "\\1")]
    | ta =~ regexReplace
    = Just
        [(replace regexReplace ta "\\1", replace regexReplaceKana taKana "\\1")]
    | ta =~ regexParentheses
    = Just
        $  [(replaceParentheses "", replaceParenthesesKana "")]
        ++ ( map
                   (\(ta, taKana) ->
                       (replaceParentheses ta, replaceParenthesesKana taKana)
                   )
           $ convertInParentheses p
           )
    | otherwise
    = Just [(ta, taKana)]
  where
    ta                     = townArea p
    taKana                 = townAreaKana p
    replaceParentheses     = replace regexParentheses ta
    replaceParenthesesKana = replace regexParenthesesKana taKana
    replace regex = subRegex (mkRegex regex)


convertInParentheses :: Postcode -> [(String, String)]
convertInParentheses p =
    filter
            (\(s, _) -> (s =~ regexUseInParentheses)
                || not (s =~ regexIgnoreInParentheses)
            )
        $ zip (splitInParentheses ta) (splitInParenthesesKana taKana)
  where
    ta     = townArea p
    taKana = townAreaKana p

splitInParentheses :: String -> [String]
splitInParentheses str = case matchParentheses str of
    Just strInParenthes -> splitOn "、" strInParenthes
    Nothing             -> []

splitInParenthesesKana :: String -> [String]
splitInParenthesesKana str =
    maybe (repeat str) (splitOn "､") $ matchParenthesesKana str

matchParentheses :: String -> Maybe String
matchParentheses = matchParentheses' regexParentheses

matchParenthesesKana :: String -> Maybe String
matchParenthesesKana = matchParentheses' regexParenthesesKana

matchParentheses' :: String -> String -> Maybe String
matchParentheses' regex = fmap head . matchRegex (mkRegex regex)

test :: String -> String -> Bool
test str regex = str =~ regex

replaceTest :: String -> String -> String
replaceTest str regex = subRegex (mkRegex regex) str "　\\1"
