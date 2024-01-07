module HelperMode where

import WordUtils hiding (filterWords)
import Data.Map hiding (foldl, map, filter)
import Data.List (intersect, find)
import qualified Data.Map as Map
import Prelude

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
                    | tp == Green = word!!idx == ch && acc
                    | tp == Yellow = ch `elem` word && acc
                    | tp == Gray = ch `notElem` withNoGreens && acc
                    | otherwise = acc

                withNoGreens = foldl 
                    (\acc (ch, idx) -> if (Green, ch, idx) `notElem` guessList then ch : acc else acc) 
                    [] $ zip word [0..]

checkForContradictions :: CharMap -> [(GuessType, Char, Int)] -> Bool
checkForContradictions guessMap = foldl charCheck False
    where
        charCheck acc (tp, ch, idx)
            | tp == Gray = ((ch, idx) `elem` yellows || (ch, idx) `elem` greens) || acc
            | tp == Green = findGreenIndex idx ch || acc
            | tp == Yellow = ((ch `elem` map fst grays && ch `notElem` map fst greens) || (ch, idx) `elem` greens) || acc
            | otherwise = acc

        findGreenIndex idx ch = case find (\(c, id) -> id == idx && c /= ch) greens of
            Just _ -> True
            Nothing -> False

        grays = Map.findWithDefault [] Gray guessMap
        yellows = Map.findWithDefault [] Yellow guessMap
        greens = Map.findWithDefault [] Green guessMap


helperMode :: CharMap -> [String] -> IO ()
helperMode guessMap words = do
    print "Enter space-separated list of Colors. (Gray, Yellow, Green)"
    let guess = head words
    print $ "My guess is: " ++ guess

    line <- getLine
    let eval = map read (Prelude.words line) :: [GuessType]

    let curr = zip3 eval guess [0..]

    let check = checkForContradictions guessMap curr

    if check then do
        print "There was a contradiction with your answers, try again"
        helperMode guessMap words
    else do
        if foldl (\acc x -> acc && x == Green) True eval 
            then print "I win"
            else helperMode (updateCharMap guessMap curr) $ filterWords (zip3 eval guess [0..]) words

startHelperMode :: [String] -> IO ()
startHelperMode words = do
    helperMode (Map.fromList [(Green, []), (Yellow, []), (Gray, [])]) words 