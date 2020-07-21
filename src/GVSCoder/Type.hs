module GVSCoder.Type where

import qualified Data.ByteString.Char8 as BS

data User = User{
    name :: BS.ByteString,
    pass :: BS.ByteString,
    --csrf :: BS.ByteString,
    --cookie :: BS.ByteString,
    contest :: BS.ByteString
}

data Judgement = AC | WA | TLE | MLE | RE | CE | QLE | IE | WJ | WR | Judging
    deriving (Eq, Show)

type SubmitResult = (Problem, Judgement)
type Problem = String
