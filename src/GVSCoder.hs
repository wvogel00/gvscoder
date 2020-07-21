{-# LANGUAGE OverloadedStrings #-}
module GVSCoder where

import Curse
import Network.HTTP.Client
import Network.HTTP.Client.TLS (tlsManagerSettings)
import qualified Data.ByteString.Char8 as BS
import Data.ByteString.Lazy.Char8 (toStrict)
import Text.Trifecta hiding (draw)
import Control.Applicative
import GVS
import Parser
import Debug.Trace (traceShowId)
import Data.Time.Clock (getCurrentTime)
import Data.Aeson (object, (.=), encode)
import GVSCoder.Type
import Control.Concurrent (threadDelay)

acPath = "https://atcoder.jp/contests/abc171/submissions/me"
gvsAgent = "gvscoder/1.0"

-- Loginリクエストの生成
makeLoginReq req user = req{
    method = "POST",
    queryString = BS.concat [
            "?csrf_token=", csrf user,
            "&username=", name user,
            "&password=", pass user
            ]
    }

attatchCookie req' cookie = do
    now <- getCurrentTime
    return . fst $ insertCookiesIntoRequest req' cookie now

startGVSCoder = do
    username <- putStrLn "username:" >> BS.getLine
    password <- putStrLn "password:" >> BS.getLine
    contestName <- putStrLn "contest name? (e.g. \"abc171\")" >> BS.getLine
    port <- readFile "portname"
    BS.putStrLn $ BS.append username "，welcome to GVS-Coder !!"

    -- setCurses
    manager <- newManager tlsManagerSettings
    initReq' <- parseRequest "https://atcoder.jp/login"
    let initReq = initReq' {requestHeaders = [("User-Agent", gvsAgent)]}
    getLoginRes <- httpLbs initReq manager
    -- BS.putStrLn.toStrict $ responseBody getLoginRes
    let cookie1 = responseCookieJar getLoginRes
    -- putStrLn.show $ "GET LoginPage : " ++ show (responseStatus getLoginRes)
    let csrfToken = findCSRFToken . toStrict $ responseBody getLoginRes
    case csrfToken of
        Success token -> do
            let user = User{name = username, pass = password, csrf= BS.pack token, contest = contestName}
            -- BS.putStr "CSRF:Token : " >> BS.putStrLn (csrf user)
            -- Cookieの挿入
            req <- attatchCookie (makeLoginReq initReq user) cookie1
            -- loginのPOST
            postLoginRes <- httpLbs req manager

            let cookie' = responseCookieJar postLoginRes
            
            putStrLn.show $ "POST LoginPage : " ++ show (responseStatus postLoginRes)
            putStrLn $ (replicate 10 '=')
            case responseStatus postLoginRes of
                status200 -> do
                    (user', status) <- getACs manager (user{cookie = cookie'})
                    loopGVS manager user' status
                _ -> BS.putStrLn "fail to authentification"
        Failure e -> BS.putStrLn "fail to get CSRF token. try again" >> print e

    -- drawLnColored 1 "GVS-Coder"
    {-
    gvs <- openGVS port
    loopGVS initRequest manager gvs name <*> getACs initRequest manager username
    -}
    -- waitAnyKey
    -- finishCurse

getACs :: Manager -> User -> IO (User, WholeStatus)
getACs manager user = do
    req'' <- parseRequest "https://atcoder.jp"
    let req' = req'' {
        path = BS.concat ["/contests/", contest user, "/submissions/me"],
        requestHeaders= [("User-Agent", gvsAgent)]
    }
    req <- attatchCookie req' (cookie user)
    res <- httpLbs req manager
    let newCookie = responseCookieJar res
    -- BS.putStrLn.toStrict.responseBody $ res
    let js = judgementP (BS.unpack $ contest user) . BS.unpack . toStrict $ responseBody res
    mapM_ (putStrLn.show) js
    return (user{cookie = newCookie}, js)

loopGVS :: Manager -> User -> WholeStatus -> IO ()
loopGVS manager user prevStatus = do
    (user', newStatus) <- getACs manager user
    if sumAc newStatus > sumAc prevStatus
        then putStrLn "Accepted!" --drawLnColored 3 "Accepted!!"
        else if any isWJ newStatus
            then putStrLn "Waiting On Judge..."
            else if countExceptAC newStatus > countExceptAC prevStatus
                --then drawLnColored 6 "Rejected!!" >> drawLnColored 1 "GVS ON" >> gvsON gvs
                then putStrLn "Rejected!!"
                else putStrLn "No change..."
    threadDelay (4*10^6) -- micro seconds
    loopGVS manager user' newStatus

isWJ = (\status -> wj status > 0).snd

sumAc, countExceptAC :: WholeStatus -> Int
sumAc = sum.map ((\status -> ac status).snd) 

countExceptAC = sum.map ((\status -> f status).snd) where
    f status = sum $ map (\g -> g status) [wa, tle, mle, re, ce, qle, ie, wr]