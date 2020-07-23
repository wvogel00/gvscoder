{-# LANGUAGE OverloadedStrings #-}
module GVSCoder where

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
import GHC.IO.Handle (hSetEcho)
import System.IO (stdin)

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
    hSetEcho stdin False
    password <- putStrLn "password:" >> BS.getLine
    hSetEcho stdin True
    contestName <- putStrLn "contest name? (e.g. \"abc001\")" >> BS.getLine
    port <- readFile "portname"
    -- open GVS
    gvs <- openGVS port
    
    -- start curses
    putStrLn $ (BS.unpack username) ++ "，welcome to GVS-Coder !!"

    manager <- newManager tlsManagerSettings
    initReq' <- parseRequest "https://atcoder.jp/login"
    let initReq = initReq' {requestHeaders = [("User-Agent", gvsAgent)]}
    getLoginRes <- httpLbs initReq manager
    let cookie = responseCookieJar getLoginRes

    -- extract CSRF token
    let csrfToken = findCSRFToken . toStrict $ responseBody getLoginRes
    case csrfToken of
        Success token -> do
            let user = User{name = username, pass = password, csrf= BS.pack token, contest = contestName}
            -- Cookieの挿入
            req <- attatchCookie (makeLoginReq initReq user) cookie
            -- loginのPOST
            postLoginRes <- httpLbs req manager

            let cookie' = responseCookieJar postLoginRes
            
            putStrLn $ (replicate 10 '=')
            case responseStatus postLoginRes of
                status200 -> do
                    (user', status) <- getACs manager (user{cookie = cookie'})
                    loopGVS gvs manager user' status
                _ -> putStrLn "fail to authentification"
        Failure e -> putStrLn "fail to get CSRF token. try again" >> print e

getACs :: Manager -> User -> IO (User, WholeStatus)
getACs manager user = do
    req'' <- parseRequest "https://atcoder.jp"
    let req' = req'' {
        path = BS.concat ["/contests/", contest user, "/submissions/me"],
        requestHeaders= [("User-Agent", gvsAgent)]
    }
    req <- attatchCookie req' (cookie user)
    res <- httpLbs req manager
    let newCookie = responseCookieJar res -- getCookie
    let js = judgementP (BS.unpack $ contest user) . BS.unpack . toStrict $ responseBody res
    return (user{cookie = newCookie}, js)

loopGVS :: GVS -> Manager -> User -> WholeStatus -> IO ()
loopGVS gvs manager user prevStatus = do
    (user', newStatus) <- getACs manager user

    if sumAc newStatus > sumAc prevStatus
        then putStrLn "Accepted!"
        else if any isWJ newStatus
            then putStrLn "Waiting On Judge..."
            else if countExceptAC newStatus > countExceptAC prevStatus
                then putStrLn "Rejected!!" >> putStrLn "GVS!!!!" >> gvsON gvs
                else putStrLn "No change..."
    threadDelay (4*10^6) -- micro seconds
    loopGVS gvs manager user' newStatus

isWJ = (\status -> wj status > 0).snd

sumAc, countExceptAC :: WholeStatus -> Int
sumAc = sum.map ((\status -> ac status).snd) 

countExceptAC = sum.map ((\status -> f status).snd) where
    f status = sum $ map (\g -> g status) [wa, tle, mle, re, ce, qle]