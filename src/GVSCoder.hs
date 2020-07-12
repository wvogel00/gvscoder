{-# LANGUAGE OverloadedStrings #-}
module GVSCoder where

import Curse
import Network.HTTP.Client
import Network.HTTP.Client.TLS (tlsManagerSettings)
import qualified Data.ByteString.Char8 as BS
import Data.ByteString.Lazy.Char8 (toStrict)
import Text.Trifecta hiding (draw)
import Text.Trifecta.Delta (Delta(..))
import Data.List (isInfixOf)
import Control.Applicative
import GVS
import Parser
import Debug.Trace (traceShowId)
import Data.Time.Clock (getCurrentTime)
import Data.Aeson (object, (.=), encode)

acPath = "https://atcoder.jp/contests/abc171/submissions/me"
gvsAgent = "gvscoder/1.0"

-- Loginリクエストの生成
makeLoginReq req name pass csrf = req{
    method = "POST",
    queryString = "",
    requestHeaders = requestHeaders req ++ [
        ("User-Agent", gvsAgent),
        ("username", BS.pack name),
        ("password", BS.pack pass)
        --("csrf_token", BS.pack csrf)
        ],
    requestBody = RequestBodyLBS $ encode reqObj
    } where
        reqObj = object ["username" .= name, "password" .= pass]--, "csrf_token" .= csrf]

-- CSRFトークンのパース
findCSRFToken :: BS.ByteString -> Result String
findCSRFToken = parseByteString csrfP (Columns 0 0).head.filter (isInfixOf "csrf_token".BS.unpack). BS.lines
    where 

startGVSCoder = do
    username <- putStrLn "username:" >> getLine
    password <- putStrLn "password:" >> getLine
    port <- putStrLn "serial port name:" >> getLine
    putStrLn $ username ++ ", welcome to GVS-Coder!"
    -- setCurses
    manager <- newManager tlsManagerSettings
    initReq' <- parseRequest "https://atcoder.jp/login"
    let initReq = initReq' {requestHeaders = [("User-Agent", gvsAgent)]}
    getLoginRes <- httpLbs initReq manager
    let gvsCookie = responseCookieJar getLoginRes
    putStrLn.show $ "GET LoginPage : " ++ show (responseStatus getLoginRes)
    let csrfToken = findCSRFToken . toStrict $ responseBody getLoginRes
    case csrfToken of
        Success token -> do
            BS.putStr "CSRF:Token : " >> BS.putStrLn (BS.pack token)
            -- Cookieの挿入
            now <- getCurrentTime
            let req' = makeLoginReq initReq username password token
            let req = fst $ insertCookiesIntoRequest req' gvsCookie now
            BS.putStrLn.BS.pack.show $  req
            -- loginのPOST
            postLoginRes <- httpLbs req manager
            putStrLn.show $ "POST LoginPage : " ++ show (responseStatus postLoginRes)
            BS.putStrLn.toStrict $ responseBody postLoginRes
            putStrLn $ replicate 20 '='
        Failure e -> BS.putStrLn "fail to get CSRF token.." >> print e

    -- drawLnColored 1 "GVS-Coder"
    {-
    gvs <- openGVS port
    loopGVS initRequest manager gvs name <*> getACs initRequest manager username
    -}
    -- waitAnyKey
    -- finishCurse

getACs req manager name = return (1,1)

loopGVS req manager gvs name (prevAC, prevOthers) = do
    (nowAC, nowOthers) <- getACs req manager name
    if nowAC > prevAC 
        then drawLnColored 3 "Accepted!!"
        else if nowOthers > prevOthers
            then drawLnColored 6 "Rejected!!" >> drawLnColored 1 "GVS ON" >> gvsON gvs
            else return ()
    loopGVS req manager gvs name (prevAC, prevOthers)