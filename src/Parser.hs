module Parser (csrfP) where

import Text.Trifecta
import Text.Trifecta.Delta (Delta(..))
import Control.Applicative

csrfP, tokenP :: Parser String
csrfP = (string "\"csrf_token\" value=\"" *> tokenP <* char '\"' ) <|> (spaces *> others *> csrfP)

tokenP = many (alphaNum <|> oneOf "=/&#;+")

others = alphaNum <|> oneOf "<>\"="