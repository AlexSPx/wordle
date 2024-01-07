module RegularMode where

import WordUtils

regularGame :: String -> Int -> IO ()
regularGame word count =
    if count == 0
        then putStrLn "You lost"
        else do
            putStrLn $ "Tries left: " ++ show count ++ ". Enter your guess:"
            guess <- getLine
            case guess of
                _ | wordLength /= length guess -> do
                    putStrLn ("The word must be of length: " ++ show wordLength)
                    regularGame word count  

                _ | word == guess -> putStrLn $ map (const '🟩') [1..wordLength]
                _ -> do
                    let eval = evalWord word guess
                    
                    putStrLn $ printEval eval

                    regularGame word (count - 1)
            where
                wordLength = length word