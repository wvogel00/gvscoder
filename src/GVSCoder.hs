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
import GVSCoder.Type

acPath = "https://atcoder.jp/contests/abc171/submissions/me"
gvsAgent = "gvscoder/1.0"

-- Loginリクエストの生成
makeLoginReq req name pass csrf = req{
    method = "POST",
    queryString = BS.concat [
            "?csrf_token=", csrf,
            "&username=", name,
            "&password=", pass
            ]
    }

-- CSRFトークンのパース
findCSRFToken :: BS.ByteString -> Result String
findCSRFToken = parseString csrfP (Columns 0 0).BS.unpack.head.filter (BS.isInfixOf "csrf_token"). BS.lines

attatchCookie req' cookie = do
    now <- getCurrentTime
    return . fst $ insertCookiesIntoRequest req' cookie now

startGVSCoder = do
    username <- putStrLn "username:" >> BS.getLine
    password <- putStrLn "password:" >> BS.getLine
    contestName <- putStrLn "contest name? (e.g. \"abc171\")" >> BS.getLine
    let user = User{name = username, pass = password, contest = contestName}
    port <- readFile "portname"
    putStrLn port
    BS.putStrLn $ BS.append (name user) "，GVS-Coderへようこそ!"

    -- setCurses
    manager <- newManager tlsManagerSettings
    initReq' <- parseRequest "https://atcoder.jp/login"
    let initReq = initReq' {requestHeaders = [("User-Agent", gvsAgent)]}
    getLoginRes <- httpLbs initReq manager
    BS.putStrLn.toStrict $ responseBody getLoginRes
    let gvsCookie = responseCookieJar getLoginRes
    putStrLn.show $ "GET LoginPage : " ++ show (responseStatus getLoginRes)
    let csrfToken = findCSRFToken . toStrict $ responseBody getLoginRes
    case csrfToken of
        Success token -> do
            BS.putStr "CSRF:Token : " >> putStrLn (token) >> BS.putStrLn (BS.pack token)
            let req' = makeLoginReq initReq username password (BS.pack token)
            -- Cookieの挿入
            req <- attatchCookie req' gvsCookie
            -- loginのPOST
            postLoginRes <- httpLbs req manager
            putStrLn.show $ "POST LoginPage : " ++ show (responseStatus postLoginRes)
            putStrLn $ (replicate 10 '=')
            case responseStatus postLoginRes of
                status200 -> loopGVS req manager user (0,0)
                _ -> BS.putStrLn "認証に失敗しました"
        Failure e -> BS.putStrLn "CSRFトークンの取得に失敗しました.." >> print e

    -- drawLnColored 1 "GVS-Coder"
    {-
    gvs <- openGVS port
    loopGVS initRequest manager gvs name <*> getACs initRequest manager username
    -}
    -- waitAnyKey
    -- finishCurse

getACs req' manager user = do
    let req = req' {path = BS.concat ["https://atcoder.jp/contests/", contest user, "/submissions/me"]}
    putStrLn $ replicate 40 '8'
    BS.putStrLn $ BS.append "リクエストパス = " $ path req
    BS.putStrLn . BS.pack.show $  req
    res <- httpLbs req manager
    BS.putStrLn.toStrict.responseBody $ res
    return (1,1)

loopGVS req manager user (prevAC, prevOthers) = do
    (nowAC, nowOthers) <- getACs req manager user
    if nowAC > prevAC 
        then putStrLn "Accepted!" --drawLnColored 3 "Accepted!!"
        else if nowOthers > prevOthers
            --then drawLnColored 6 "Rejected!!" >> drawLnColored 1 "GVS ON" >> gvsON gvs
            then putStrLn "Rejected!!"
            else return ()
    -- loopGVS req manager gvs name (nowAC, nowOthers)