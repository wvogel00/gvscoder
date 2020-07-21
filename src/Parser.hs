module Parser (csrfP) where

import Text.Trifecta
import Text.Trifecta.Delta (Delta(..))
import Control.Applicative
import GVSCoder.Type

csrfP, tokenP :: Parser String
csrfP = format <$> (string "\"csrf_token\" value=\"" *> tokenP <* char '\"' ) <|> (spaces *> others *> csrfP)

tokenP = many (alphaNum <|> oneOf "=/&#;+")

-- レスポンスを取得した時点でCSRF中の'+'文字が"&#43;"に文字化けしてしまっている．これを"%2Bに置き換える"
format [] = []
format xs = if take 5 xs == "&#43;" then "%2B"++format (drop 5 xs) else head xs : format (tail xs)

others = alphaNum <|> oneOf "<>\"="

judgementP :: Parser SubmitResult
judgementP = return ("B", AC)