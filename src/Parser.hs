module Parser (csrfP) where

import Text.Trifecta
import Text.Trifecta.Delta (Delta(..))
import Control.Applicative

csrfP, tokenP :: Parser String
csrfP = format <$> (string "\"csrf_token\" value=\"" *> tokenP <* char '\"' ) <|> (spaces *> others *> csrfP)

tokenP = many (alphaNum <|> oneOf "=/&#;+")

format [] = []
format xs = if take 5 xs == "&#43;" then "%2B"++format (drop 5 xs) else head xs : format (tail xs)

others = alphaNum <|> oneOf "<>\"="