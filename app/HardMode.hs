module HardMode where

import WordUtils
import Data.Map hiding (foldl, map)
import qualified Data.Map as Map
import System.Random (randomRIO)
import Data.List (intersect)

type CharMap = Map GuessType [(Char, Int)]

updateCharMap :: CharMap -> [(GuessType, Char, Int)] -> CharMap
updateCharMap = foldl updateList
    where
        updateList acc (key, ch, idx) = Map.adjust ((ch, idx) :) key acc

makeLie :: CharMap -> String -> IO [GuessType]
makeLie guessMap word = do
    let grays = Map.findWithDefault [] Gray guessMap
        yellows = Map.findWithDefault [] Yellow guessMap
        greens = Map.findWithDefault [] Green guessMap
 
        lieCheck (ch, idx)
            | ch `elem` map fst greens =
                if (ch, idx) `elem` greens
                    then return Green
                    else return Yellow
            | ch `elem` map fst yellows =
                if (ch, idx) `elem` yellows
                    then return Yellow
                    else randomElement $ [Green, Yellow] `intersect` alls
            | ch `elem` map fst grays = return Gray
            | otherwise = do
                randomElement $ [Gray, Yellow, Green] `intersect` alls
            where
                alls = if idx `elem` map snd greens then [Yellow, Gray] else [Gray, Yellow, Green] 

    mapM lieCheck (zip word [0..])

hardMode :: CharMap -> String -> Int -> Bool -> IO ()
hardMode charMap word tries lied = do
    if tries == 0
        then putStrLn "You lost"
        else do
            putStrLn $ "Tries left: " ++ show tries ++ ". Enter your guess:"
            guess <- getLine
            case guess of
                _ | word == guess -> putStrLn $ map (const '🟩') [1..wordLength]
                _ -> do
                    seed <- randomRIO (0, 1) :: IO Int

                    if not lied && seed == 1 
                        then do
                            eval <- makeLie charMap guess
                            
                            print eval
                            putStrLn $ printEval eval
                            putStrLn "LEID"

                            hardMode charMap word (tries - 1) True
                        else do
                            let eval = evalWord word guess
                            
                            putStrLn $ printEval eval

                            hardMode (updateCharMap charMap $ zip3 eval guess [0..]) word (tries - 1) lied
            where
                wordLength = length word

startHardGame :: String -> Int -> IO ()
startHardGame word tries = do
    hardMode (Map.fromList [(Green, []), (Yellow, []), (Gray, [])]) word tries False