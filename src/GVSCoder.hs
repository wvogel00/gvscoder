{-# LANGUAGE OverloadedStrings #-}
module GVSCoder where

import Curse
import Network.HTTP.Client
import Network.HTTP.Client.TLS (tlsManagerSettings)
import qualified Data.ByteString.Char8 as BS
import Data.ByteString.Lazy.Char8 (toStrict)
import Text.Trifecta hiding (draw)
import Data.List (isInfixOf)
import Control.Applicative
import GVS
import Parser

acPath = "https://atcoder.jp/contests/abc171/submissions/me"

getLoginPage req manager = do
    response <- httpLbs req manager
    return response

postLoginPage baseReq manager name pass csrf = do
    let req = baseReq{
                    method = "POST",
                    queryString = "",
                    requestHeaders = [
                    --    ("username",name),
                      --  ("password",pass),
                        ("csrf_token",BS.pack csrf)],
                    requestBody = ""
                    }
    res <- httpLbs req manager
    return res

findCSRFToken :: BS.ByteString -> Result String
findCSRFToken = parseByteString csrfP (Columns 0 0).head.filter (isInfixOf "csrf_token".BS.unpack). BS.lines

startGVSCoder = do
    username <- putStrLn "username:" >> BS.getLine
    password <- putStrLn "password:" >> BS.getLine
    port <- putStrLn "serial port name:" >> BS.getLine
    setCurses
    manager <- newManager tlsManagerSettings
    initialRequest <- parseRequest "https://atcoder.jp/login"
    getLoginRes <- getLoginPage initialRequest manager
    drawLn.show $ "GET LoginPage : " ++ show (responseStatus getLoginRes)
    let csrfToken = findCSRFToken . toStrict $ responseBody getLoginRes
    case csrfToken of
        Success token -> do
            drawLn token
            postLoginRes <- postLoginPage initialRequest manager username password token
            drawLn.show $ "POST LoginPage : " ++ show (responseStatus postLoginRes)
            drawLn $ replicate 20 '='
            drawLn.show $ responseBody postLoginRes
        Failure e -> BS.putStrLn "fail to get CSRF token.." >> print e

    drawLnColored 1 "GVS-Coder"
    drawLnColored 1 $ "CSRF:Token" ++ show csrfToken
    drawLnColored 1 $ replicate 20 '-'
    {-
    gvs <- openGVS port
    loopGVS initRequest manager gvs name <*> getACs initRequest manager username
    -}
    waitAnyKey
    finishCurse

getACs req manager name = do
    return (1,1)

loopGVS req manager gvs name (prevAC, prevOthers) = do
    (nowAC, nowOthers) <- getACs req manager name
    if nowAC > prevAC 
        then drawLnColored 3 "Accepted!!"
        else if nowOthers > prevOthers
            then drawLnColored 6 "Rejected!!" >> drawLnColored 1 "GVS ON" >> gvsON gvs
            else return ()
    loopGVS req manager gvs name (prevAC, prevOthers)