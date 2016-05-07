module Main (main) where

import Data.List (intercalate)
import Data.Map.Strict (findWithDefault, fromListWith)
import Data.Maybe (mapMaybe)
import Text.Regex.TDFA ((=~~))


main = do
    kanjiWordsFile <- readFile "deck.txt"
    let dict = fromListWith (++) $ reverse $ concat $ map (parseWordInfo . getGroupResults) $ mapMaybe parseNote $ lines kanjiWordsFile
    kanjiFile <- readFile "kanji_info.txt"
    let kanjiFileWithWords = unlines $ map (addDictInfo dict) $ lines kanjiFile
    writeFile "kanji_more_info.txt" kanjiFileWithWords
        where parseWordInfo wordInfo = map (\kanji -> (kanji, [wordInfoFromList wordInfo])) (wordInfo!!0)
              getGroupResults = tail . (!!0)
              addDictInfo dict line@(kanji:_) = line ++ " [" ++ intercalate ", " (map show $ findWithDefault [] kanji dict) ++ "]"

parseNote :: String -> Maybe [[String]]
parseNote str = str =~~ "^[0-9]+\t([^\t]+)\t([^\t]+)\t([^\t]+)"

data WordInfo = WordInfo String String String

instance Show WordInfo where
    show (WordInfo kanji kana def) = kanji ++ " (" ++ kana ++ "|" ++ def ++ ")"

wordInfoFromList :: [String] -> WordInfo
wordInfoFromList (kanji:kana:def:_) = WordInfo kanji kana def
