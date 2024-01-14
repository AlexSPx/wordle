module Main where

import WordUtils
import EasyMode
import RegularMode
import HardMode
import HelperMode

import System.IO (hPutStrLn, hSetEncoding, stdout, utf8)
import System.Win32.Console (setConsoleOutputCP)
import Data.Char (toLower)

main :: IO ()
main = do
    -- https://discourse.elm-lang.org/t/help-improve-unicode-support-on-windows/3366/3
    hSetEncoding stdout utf8 -- This has to be the first action, otherwise you get the "invalid character" error
    setConsoleOutputCP 65001 -- The "Output" variant is for printing, the other one is for console input. Probably want to set both

    putStrLn "Enter word length (1-28)"
    sizeInput <- getLine 

    words <- WordUtils.loadWords (read sizeInput :: Int)
    word <- WordUtils.randomElement words
    putStrLn "Wordle - Haskell project"
    
    putStrLn "Gamemodes:"
    putStrLn "0 - Regular Game"
    putStrLn "1 - Easy Game"
    putStrLn "2 - Hard Game"
    putStrLn "3 - Helper Mode"
    mode <- getLine
    
    case mode of
        "0" -> regularGame word 6
        "1" -> startEasyGame words word 6
        "2" -> startHardGame word 6
        "3" -> startHelperMode word 6 words
