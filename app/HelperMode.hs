module HelperMode where

import WordUtils hiding (filterWords)
import Data.Map hiding (foldl, foldr, map, filter, (\\), null)
import Data.List (intersect, find, (\\), nub)
import qualified Data.Map as Map
import Prelude
import Control.Monad (filterM)

type CharMap = Map GuessType [(Char, Int)]

updateCharMap :: CharMap -> [(GuessType, Char, Int)] -> CharMap
updateCharMap = foldl updateList
    where
        updateList acc (key, ch, idx) = Map.adjust ((ch, idx) :) key acc

filterWords :: [(GuessType, Char, Int)] -> [String] -> [String]
filterWords guessList = filter validateWord
    where
        validateWord word = foldl checkContains True guessList
            where
                checkContains acc (tp, ch, idx)
                    | tp == Green = word !! idx == ch && acc
                    | tp == Yellow = ch `elem` word && word !! idx /= ch && acc
                    | tp == Gray = ch `notElem` withNoGY && acc
                    | otherwise = acc

                withNoGY = foldr
                    (\(ch, idx) acc -> if (Green, ch, idx) `notElem` guessList 
                        && (Prelude.null $ filter (\(tpe, char, _) -> tpe == Yellow && char == ch) guessList) then ch : acc else acc) 
                    [] $ zip word [0..]
        
mostEffectiveWords :: [String] -> [String]
mostEffectiveWords words = filter 
    (\word -> length (nub word) == maxLen && countVolwels (nub word) == maxVowels) 
    words
    where
        maxLen = maximum $ map (length . nub) words
        maxVowels = maximum $ map (countVolwels . nub) words

        countVolwels :: String -> Int
        countVolwels = length . filter (`elem` "aeiou")

helperMode :: Int -> CharMap -> [String] -> IO ()
helperMode tries guessMap words = do
    if tries == 0 then
        print "I lost"
    else 
        if null words then do
            print "There was a contradiction with your answers, try again"
        else do
            putStrLn $ show tries ++ " tries left. Enter space-separated list of Colors. (Gray, Yellow, Green)"
            guess <- randomElement $ mostEffectiveWords words
            print $ "My guess is: " ++ guess

            line <- getLine
            let eval = map read (Prelude.words line) :: [GuessType]

            let curr = zip3 eval guess [0..]


            if foldl (\acc x -> acc && x == Green) True eval 
                then print "I win"
                else helperMode (tries - 1) (updateCharMap guessMap curr) $ filterWords (zip3 eval guess [0..]) words


startHelperMode :: String -> Int -> [String] -> IO ()
startHelperMode word tries words = do
    print $ "The word is: " ++ word
    helperMode tries (Map.fromList [(Green, []), (Yellow, []), (Gray, [])]) words 