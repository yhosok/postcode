{-# LANGUAGE DeriveGeneric #-}

module Lib where

import qualified Data.ByteString.Lazy          as BL
import           Data.Csv
import qualified Data.Vector                   as V
import           Text.Regex.TDFA hiding ((!), (*>)) -- Bring in TDFA, hide conflicting operators if any
import           Text.Regex.TDFA.Text () -- For using TDFA with Data.Text.Text
import           Text.Regex.TDFA.String () -- For using TDFA with String
import           System.IO
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as TIO
import qualified Data.Array                    as Array -- Explicit import for Data.Array
import qualified Data.Text.Encoding            as TE
import qualified Data.Text.Encoding.Error      as TEE
import           Data.List.Split
import           GHC.Generics                   ( Generic )
import           Data.Maybe                     ( fromJust, isJust )
import           Data.Array ((!)) -- For accessing match groups

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
regexFloor = "（(.+階)）" -- Allow any characters before 階

regexFloorKana :: String
regexFloorKana = "\\((.+ｶｲ)\\)" -- Allow any characters before ｶｲ

regexJiwari :: String
regexJiwari = "^([^０-９第（]+)[第]*[０-９]+地割.*"

regexJiwariKana :: String
regexJiwariKana = "^([^0-9\\(]+)(ﾀﾞｲ)*[0-9]*ﾁﾜﾘ.*" -- Reverted to original, simpler form

regexUseInParentheses :: String
regexUseInParentheses = "[０-９]+区"

regexUseInParenthesesKana :: String
regexUseInParenthesesKana = "[0-9]+ｸ"

regexIgnoreInParentheses :: String
regexIgnoreInParentheses =
    "[０-９]|^その他$|^丁目$|^番地$|^地階・階層不明$|[０-９]*地割|成田国際空港内|次のビルを除く|^全域$"

convert :: FilePath -> FilePath -> IO ()
convert input output = do
    -- Open the file with Shift_JIS encoding
    h <- openFile input ReadMode
    encoding <- mkTextEncoding "SHIFT_JIS"
    hSetEncoding h encoding
    -- Read as Text
    textContent <- TIO.hGetContents h
    -- Encode to UTF-8 ByteString for Cassava
    let csvData = BL.fromStrict $ TE.encodeUtf8 textContent
    -- Close the handle
    hClose h

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
    nextLastChanged = case targetToWrite of
        []    -> lastChanged
        (h:_) -> if isSamePostcode h then lastChanged else Just h

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
    -- regexFloor must be checked before regexParentheses
    | ta =~ regexFloor
    = [(replace regexFloor ta "　\\1", replace regexFloorKana taKana "\\1")]
    | ta =~ regexJiwari =
        let zenkakuTown = replace regexJiwari ta "\\1"
            rawKanaTown = replace regexJiwariKana taKana "\\1"
            finalKanaTown = if T.pack "ﾀﾞｲ" `T.isSuffixOf` T.pack rawKanaTown
                            then T.unpack $ T.dropEnd (T.length (T.pack "ﾀﾞｲ")) (T.pack rawKanaTown)
                            else rawKanaTown
        in [(zenkakuTown, finalKanaTown)]
    | ta =~ regexParentheses =
        let processed = converAndReplace p
        in if null processed then [("", "")] else processed
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
    -- replace regex = subRegex (mkRegex regex) -- Old function using Text.Regex

-- New replace function using Text.Regex.TDFA
replace :: String -> String -> String -> String
replace patternStr inputStr replacementStr =
    let compileOpts = defaultCompOpt { lastStarGreedy = True }
        executeOpts = defaultExecOpt
        -- TDFA works with Text, so we convert String to Text and back
        regex = makeRegexOpts compileOpts executeOpts (T.pack patternStr)

        go :: T.Text -> T.Text -- Accumulator for the result, operating on Text
        go currentInputText =
            case matchOnceText regex currentInputText of
                Just (pre, match, post) ->
                    let matchArray = match -- 'match' is already MatchText T.Text
                        -- Backreference replacement needs to operate on Text
                        processedReplacement = replaceBackrefs (T.pack replacementStr) matchArray
                    in T.concat [pre, processedReplacement, go post]
                Nothing -> currentInputText -- No more matches

        -- Helper to replace \1, \2 etc. (simplified, operates on Text)
        replaceBackrefs :: T.Text -> MatchText T.Text -> T.Text
        replaceBackrefs rplStr matchGroupsArray = T.pack $ goReplace (T.unpack rplStr) (map (T.unpack . fst) (Array.elems matchGroupsArray))
          where
            goReplace [] _ = []
            goReplace ('\\':'0':xs) arr = if null arr then "" else (arr !! 0) ++ goReplace xs arr
            goReplace ('\\':'1':xs) arr = if length arr < 2 then "" else (arr !! 1) ++ goReplace xs arr
            goReplace ('\\':'2':xs) arr = if length arr < 3 then "" else (arr !! 2) ++ goReplace xs arr
            goReplace ('\\':'3':xs) arr = if length arr < 4 then "" else (arr !! 3) ++ goReplace xs arr
            goReplace ('\\':'4':xs) arr = if length arr < 5 then "" else (arr !! 4) ++ goReplace xs arr
            goReplace ('\\':'5':xs) arr = if length arr < 6 then "" else (arr !! 5) ++ goReplace xs arr
            goReplace ('\\':'6':xs) arr = if length arr < 7 then "" else (arr !! 6) ++ goReplace xs arr
            goReplace ('\\':'7':xs) arr = if length arr < 8 then "" else (arr !! 7) ++ goReplace xs arr
            goReplace ('\\':'8':xs) arr = if length arr < 9 then "" else (arr !! 8) ++ goReplace xs arr
            goReplace ('\\':'9':xs) arr = if length arr < 10 then "" else (arr !! 9) ++ goReplace xs arr
            goReplace ('\\':'\\':xs) arr = '\\' : goReplace xs arr
            goReplace (x:xs) arr = x : goReplace xs arr

    in T.unpack $ go (T.pack inputStr)

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

-- Updated matchParentheses' using Text.Regex.TDFA
matchParentheses' :: String -> String -> Maybe String
matchParentheses' patternStr str =
    let -- Compile the regex with explicit types.
        compOpts :: CompOption
        compOpts = defaultCompOpt
        execOpts :: ExecOption
        execOpts = defaultExecOpt
        regex :: Regex
        regex = makeRegexOpts compOpts execOpts patternStr
        -- Perform the match. matchM returns Maybe (MatchResult String).
        maybeMatchResult :: Maybe (MatchResult String)
        maybeMatchResult = matchM regex str
    in case maybeMatchResult of
         Just matchResultInstance ->
             -- Extract submatches. mrSubs gives MatchText String, which is Array Int String.
             let subMatches :: Array.Array Int String
                 subMatches = mrSubs matchResultInstance
                 -- Groups are typically indexed from 1. Group 0 is the whole match.
                 -- We want the first captured group, so index 1.
                 numberOfGroups = Array.rangeSize . Array.bounds $ subMatches
             in if numberOfGroups > 1 -- Index 0 is whole match, index 1 is first group
                then Just (subMatches ! 1) 
                else Nothing -- No capture groups or only whole match
         Nothing -> Nothing
