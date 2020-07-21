{-# LANGUAGE OverloadedStrings #-}
module Parser (csrfP, judgementP, findCSRFToken) where

import Text.Trifecta
import Text.Trifecta.Delta (Delta(..))
import Control.Applicative
import GVSCoder.Type
import Data.List
import qualified Data.ByteString.Char8 as BS

-- CSRFトークンのパース
---------------------------------------------------------------------------
findCSRFToken :: BS.ByteString -> Result String
findCSRFToken = parseString csrfP (Columns 0 0).BS.unpack.head.filter (BS.isInfixOf "csrf_token"). BS.lines

csrfP, tokenP :: Parser String
csrfP = format <$> (string "\"csrf_token\" value=\"" *> tokenP <* char '\"' ) <|> (spaces *> others *> csrfP)

tokenP = many (alphaNum <|> oneOf "=/&#;+")

-- レスポンスを取得した時点でCSRF中の'+'文字が"&#43;"に文字化けしてしまっている．これを"%2Bに置き換える"
format [] = []
format xs = if take 5 xs == "&#43;" then "%2B"++format (drop 5 xs) else head xs : format (tail xs)

others = alphaNum <|> oneOf "<>\"="
---------------------------------------------------------------------------

-- 問題番号と結果の取得
---------------------------------------------------------------------------
judgementP :: String -> String -> WholeStatus
judgementP contest =  foldl joinJudges [] . map judgementP' . extractJudgeText contest where
    judgementP' :: (String,String) -> ProblemStatus
    judgementP' (prob, judge) = (extractProblemInitial contest prob , addTag judge)

joinJudges :: WholeStatus -> ProblemStatus -> WholeStatus
joinJudges xs (prob, judge) = case find ((==prob).fst) xs of
    Just d  -> (prob, updateStatus (snd d) judge) : delete d xs
    Nothing -> (prob, updateStatus initStatus judge) : xs

extractJudgeText contest = makeTuple.filter (\x -> problemNameF x || judgementF x).lines where
    problemNameF = isInfixOf $"/contests/" ++ contest ++ "/tasks/"
    judgementF = isInfixOf "data-placement"
    makeTuple [] = []
    makeTuple (prob:judge:xs) = (prob,judge) : makeTuple xs
---------------------------------------------------------------------------

updateStatus status tag = case tag of
    AC      -> status{ac = ac status+1}
    WA      -> status{wa = wa status+1}
    TLE     -> status{tle = tle status+1}
    MLE     -> status{mle = mle status+1}
    RE      -> status{re = re status+1}
    CE      -> status{ce = ce status+1}
    QLE     -> status{qle = qle status+1}
    IE      -> status{ie = ie status+1}
    WJ      -> status{wj = wj status+1}
    WR      -> status{wr = wr status+1}
    Judging -> status{judging = judging status+1}

extractProblemInitial contest str
    | (contest ++ "_a" ) `isInfixOf` str = "A"
    | (contest ++ "_b" ) `isInfixOf` str = "B"
    | (contest ++ "_c" ) `isInfixOf` str = "C"
    | (contest ++ "_d" ) `isInfixOf` str = "D"
    | (contest ++ "_e" ) `isInfixOf` str = "E"
    | (contest ++ "_f" ) `isInfixOf` str = "F"

addTag str
    | "AC"      `isInfixOf` str = AC
    | "WA"      `isInfixOf` str = WA
    | "TLE"     `isInfixOf` str = TLE
    | "MLE"     `isInfixOf` str = MLE
    | "RE"      `isInfixOf` str = RE
    | "CE"      `isInfixOf` str = CE
    | "QLE"     `isInfixOf` str = QLE
    | "IE"      `isInfixOf` str = IE
    | "WJ"      `isInfixOf` str = WJ
    | "WR"      `isInfixOf` str = WR
    | "Judging" `isInfixOf` str = Judging