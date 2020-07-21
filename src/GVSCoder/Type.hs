module GVSCoder.Type where

import Network.HTTP.Client (CookieJar)
import qualified Data.ByteString.Char8 as BS

data User = User{
    name :: BS.ByteString,
    pass :: BS.ByteString,
    csrf :: BS.ByteString,
    cookie :: CookieJar,
    contest :: BS.ByteString
} deriving (Eq,Show)

data JudgeStatus = JudgeStatus{
    ac :: Int,
    wa :: Int,
    tle :: Int,
    mle :: Int,
    re :: Int,
    ce :: Int,
    qle :: Int,
    ie :: Int,
    wj :: Int,
    wr :: Int,
    judging :: Int
} deriving (Eq,Show)

data Judgement = AC | WA | TLE | MLE | RE | CE | QLE | IE | WJ | WR | Judging
    deriving (Eq, Show)

type ProblemStatus = (Problem, Judgement)
type WholeStatus = [(Problem, JudgeStatus)]
type Problem = String

initStatus = JudgeStatus{
    ac = 0, wa = 0, tle = 0, mle = 0, re = 0, ce = 0, qle = 0, ie = 0, wj = 0, wr = 0, judging = 0
}