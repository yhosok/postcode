{-# LANGUAGE DeriveGeneric #-}

module Lib where

import qualified Data.ByteString.Lazy          as BL
import           Data.Csv
import qualified Data.Vector                   as V
import           Text.Regex.TDFA
import           Text.Regex
import           System.IO
import           Data.List.Split
import           GHC.Generics                   ( Generic )
import           Data.Maybe                     ( fromJust )

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
    deriving (Generic, Show, Eq)

instance FromRecord Postcode
instance ToRecord Postcode

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

regexJiwari :: String
regexJiwari = "^([^０-９第（]+)[第]*[０-９]+地割.*"

regexJiwariKana :: String
regexJiwariKana = "^([^0-9\\(]+)(ﾀﾞｲ)*[0-9]*ﾁﾜﾘ.*"

regexUseInParentheses :: String
regexUseInParentheses = "[０-９]+区"

regexUseInParenthesesKana :: String
regexUseInParenthesesKana = "[0-9]+ｸ"

regexIgnoreInParentheses :: String
regexIgnoreInParentheses =
    "[０-９]|^その他$|^丁目$|^番地$|^地階・階層不明$|[０-９]*地割|成田国際空港内|次のビルを除く|^全域$"

convert :: FilePath -> FilePath -> IO ()
convert input output = do
    csvData <- BL.readFile input
    BL.writeFile output BL.empty
    case decode NoHeader csvData of
        Left  err     -> putStrLn err
        Right records -> V.foldM_ (convert' output) (Nothing, Nothing) records

convert'
    :: FilePath
    -> (Maybe Postcode, Maybe Postcode)
    -> Postcode
    -> IO (Maybe Postcode, Maybe Postcode)
convert' output (lastUnClosed, lastChanged) current 
    | isUnClosed target = return (Just target, lastChanged)
    | otherwise = write targetToWrite >> return (Nothing, nextLastChanged)
  where
    target = maybe current (const merged) lastUnClosed
    merged = current
        { townArea = (townArea . fromJust $ lastUnClosed) ++ townArea current
        , townAreaKana = (townAreaKana . fromJust $ lastUnClosed)
                             ++ nextUnClosedKana
        }
    targetToWrite = filter isNotDup $ postCodeWithTownArea target
    isNotDup p = maybe True (not . isSameAddress p) lastChanged
    nextUnClosedKana
        | isUnClosedKana . fromJust $ lastUnClosed = townAreaKana current
        | otherwise                               = ""
    isUnClosed p = townArea p =~ regexUnClosedParentheses :: Bool
    isUnClosedKana p = townAreaKana p =~ regexUnClosedParenthesesKana :: Bool
    write = BL.appendFile output . encode
    isSamePostcode p =
        maybe False (\lp -> postcode lp == postcode p) lastChanged
    nextLastChanged | null targetToWrite = lastChanged
                    | isSamePostcode $ head targetToWrite = lastChanged
                    | otherwise = Just $ head targetToWrite

isSameAddress :: Postcode -> Postcode -> Bool
isSameAddress p p' = all (\f -> f p == f p') fields
  where
    fields =
        [ postcode
        , prefectureKana
        , cityKana
        , townAreaKana
        , prefecture
        , city
        , townArea
        ]

postCodeWithTownArea :: Postcode -> [Postcode]
postCodeWithTownArea p =
    map (\(ta, taKana) -> p { townArea = ta, townAreaKana = taKana })
        $ convertTownArea p

convertTownArea :: Postcode -> [(String, String)]
convertTownArea p
    | ta =~ regexIgnore
    = [("", "")]
    | ta =~ regexFloor
    = [(replace regexFloor ta "　\\1", replace regexFloorKana taKana "\\1")]
    | ta =~ regexJiwari
    = [(replace regexJiwari ta "\\1", replace regexJiwariKana taKana "\\1")]
    | ta =~ regexParentheses
    = [(replaceParentheses "", replaceParenthesesKana "")] ++ converAndReplace p
    | otherwise
    = [(ta, taKana)]
  where
    ta               = townArea p
    taKana           = townAreaKana p
    converAndReplace = map replaceParentheses' . convertInParentheses
    replaceParentheses' (ta', taKana') =
        (replaceParentheses ta', replaceParenthesesKana taKana')
    replaceParentheses     = replace regexParentheses ta
    replaceParenthesesKana = replace regexParenthesesKana taKana
    replace regex = subRegex (mkRegex regex)


convertInParentheses :: Postcode -> [(String, String)]
convertInParentheses p = filter shouldUse
    $ zip (splitInParentheses ta) (splitInParenthesesKana taKana)
  where
    shouldUse :: (String, String) -> Bool
    shouldUse (s, _) =
        s =~ regexUseInParentheses || not (s =~ regexIgnoreInParentheses)
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
