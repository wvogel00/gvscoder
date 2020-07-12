module Curse where

import qualified UI.HSCurses.Curses as C
import qualified UI.HSCurses.CursesHelper as CH

setCurses = do
    C.initCurses
    CH.start
    C.startColor
    C.initPair (C.Pair 1) CH.red C.defaultBackground
    C.initPair (C.Pair 2) CH.yellow C.defaultBackground
    C.initPair (C.Pair 3) CH.green C.defaultBackground
    C.initPair (C.Pair 4) CH.cyan C.defaultBackground
    C.initPair (C.Pair 5) CH.blue C.defaultBackground
    C.initPair (C.Pair 6) CH.magenta C.defaultBackground
    C.initPair (C.Pair 7) CH.white C.defaultBackground

waitAnyKey = C.getCh >> return ()

getKey = do
    key <- C.getCh
    case key of
        C.KeyChar c -> return (Just c)
        _ -> return Nothing

drawColored color text = do
    C.attrSet (C.attr0) (C.Pair color)
    C.wAddStr C.stdScr text
    C.refresh

drawLnColored color text = drawColored color text >> C.addLn

finishCurse = CH.end >> C.endWin

draw str = C.wAddStr C.stdScr str >> C.refresh

drawLn str = draw str >> C.addLn
