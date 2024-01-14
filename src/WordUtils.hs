module WordUtils where

import System.IO
import Prelude
import System.Random
import Data.Char (toLower)
import Data.List ((\\), nub)
import Data.IORef

filterWords :: Int -> [String] -> [String]
filterWords len words = [map toLower word | word <- words, length word == len]

loadWords :: Int -> IO [String]
loadWords n = do
    contents <- readFile "data/words.txt"
    return $ filterWords n $ lines contents

randomElement :: [a] -> IO a
randomElement xs = do
    index <- randomRIO (0, length xs - 1) :: IO Int
    return (xs !! index)

data GuessType = Green | Yellow | Gray
    deriving (Eq, Ord, Show, Read)

printEval :: [GuessType] -> [Char]
printEval [] = []
printEval (x:xs)
    | x == Green = '🟩' : printEval xs
    | x == Yellow = '🟨' : printEval xs
    | x == Gray = '⬜' : printEval xs

deleteFirst _ [] = []
deleteFirst a (b:bc) | a == b    = bc
                     | otherwise = b : deleteFirst a bc

evalWord :: String -> String -> [GuessType]
evalWord word guess = evalChar withNoGreens $ zip guess [0..]
    where
        evalChar :: [Char] -> [(Char, Int)] -> [GuessType]
        evalChar _ [] = []
        evalChar noGreens ((ch, idx) : xs)
           | word!!idx == ch        = Green : evalChar noGreens xs
           | ch `elem` noGreens     = Yellow : evalChar (deleteFirst ch noGreens) xs
           | otherwise              = Gray : evalChar noGreens xs

        withNoGreens :: [Char]
        withNoGreens = filter (/= '-') $ zipWith (\c1 c2 -> if c1 /= c2 then c1 else '-') word guess