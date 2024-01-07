module EasyMode where

import WordUtils
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Foldable (find)
import Data.Maybe (isJust)

insertIntoList :: Ord k => k -> a -> Map.Map k [a] -> Map.Map k [a]
insertIntoList key newValue =
  Map.adjust (newValue :) key

removeFromListChar :: (Ord k, Eq a) => k -> a -> Map.Map k [(a, Int)] -> Map.Map k [(a, Int)]
removeFromListChar key toRemove = Map.adjust (filter (\(ch, _) -> ch /= toRemove)) key

removeFromList :: (Ord k, Eq a) => k -> a -> Map.Map k [a] -> Map.Map k [a]
removeFromList key toRemove = Map.adjust (filter (/= toRemove)) key

checkWord :: Map.Map GuessType [(Char, Int)] -> String -> Map.Map GuessType [(Char, Int)]
checkWord warns word = Prelude.foldl checkChar initialMap $ zip word [0..]
    where
        initialMap = Map.fromList [
            (Green, Map.findWithDefault [] Green warns), 
            (Yellow, Map.findWithDefault [] Yellow warns ++ Map.findWithDefault [] Green warns), 
            (Gray, [])]

        checkChar acc r@(ch, idx)
            | isGray ch = insertIntoList Gray r $ removeFromListChar Yellow ch acc
            | otherwise = removeFromList Green r $ removeFromListChar Yellow ch acc
            
            where
                lookIn idx ch = case Map.lookup idx warns of
                    Just list   -> find (\(x,_) -> x == ch) list
                    Nothing     -> Nothing
                isGray ch = isJust $ lookIn Gray ch

                isGreen tuple = case Map.lookup Green warns of
                    Just list   -> isJust $ find (\(x,_) -> x == ch) list
                    Nothing     -> False 
                
showWarings :: Map.Map GuessType [(Char, Int)] -> IO ()
showWarings guessMap = do
    case Map.lookup Green guessMap of
        Just chars  -> print ("The following chars are not on the correct place" ++ show chars)  
        Nothing     -> print ""

    case Map.lookup Gray guessMap of
        Just chars  -> print ("The following chars are not in the word " ++ show chars)  
        Nothing     -> print ""

    case Map.lookup Yellow guessMap of
        Just chars  -> print ("You are missing the following yellow chars " ++ show chars)  
        Nothing     -> print ""

updateGuessMap :: Map.Map GuessType [(Char, Int)] -> [(GuessType, (Char, Int))] -> Map.Map GuessType [(Char, Int)]
updateGuessMap = foldl updateList
    where
        updateList :: Map.Map GuessType [(Char, Int)] -> (GuessType, (Char, Int)) -> Map.Map GuessType [(Char, Int)]
        updateList acc (key, val) =
            case Map.lookup key acc of
                Just list -> if val `elem` list then acc else insertIntoList key val acc
                Nothing -> acc

easyMode :: Map.Map GuessType [(Char, Int)] -> String -> Int -> IO ()
easyMode guessMap word tries = do
    if tries == 0 then 
        putStrLn "You lost"
    else do
        putStrLn $ "Tries left: " ++ show tries ++ ". Enter your guess:"
        guess <- getLine

        case guess of
                _ | word == guess -> putStrLn $ map (const '🟩') [1..wordLength]
                _ -> do
                    let eval = evalWord word guess
                    putStrLn $ printEval eval
                        
                    showWarings $ checkWord guessMap guess

                    easyMode (updateGuessMap guessMap $ zip eval $ zip guess [0..]) word (tries - 1)
            where
                wordLength = length word

startEasyGame :: String -> Int -> IO ()
startEasyGame = easyMode (Map.fromList [(Green, []), (Yellow, []), (Gray, [])])