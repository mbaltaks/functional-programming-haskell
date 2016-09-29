import System.Random
import System.IO
import Control.Monad
import System.IO.Unsafe


check :: String -> String -> Char -> (Bool, String)
check word display c
    = (c `elem` word, [if x==c
        then c
        else y | (x,y) <- zip word display])


turn :: String -> String -> Int -> IO ()
turn word display n =
    do if n==0
        then putStrLn "You lose"
        else if word==display
            then putStrLn "You win!"
            else mkguess word display n


mkguess :: String -> String -> Int -> IO ()
mkguess word display n =
    do
        putStrLn (display ++ "  " ++ take n (repeat '*'))
        putStr "  Enter your guess: "
        q <- getLine
        let (correct, display') = check word display (q!!0)
        let n' = if correct then n else n-1
        turn word display' n'


hidden_word :: String -> String
hidden_word word = ['-' | x <- word]


starman :: String -> Int -> IO ()
starman word n = turn word (hidden_word word) n

--
dictionary_unsafe :: String
dictionary_unsafe = unsafePerformIO . readFile $ "/usr/share/dict/words"

dictionary_action :: IO String
dictionary_action = readFile "/usr/share/dict/words"

random_number_action :: IO Int
random_number_action = randomRIO (0, 235886)

-- line_number :: Int
-- -- line_number = n <- getStdRandom (randomR (0, 235886)) n
-- -- line_number = random_number >>= return . rand_number
-- line_number = do
--     -- a <- random_number
--     let a = random_number
--     return $ rand_number a

-- rand_number :: Int -> Int
-- rand_number n = n

word_from_list :: Int -> String -> String
word_from_list line_number word_list = (lines word_list) !! line_number
    -- do
    --     let words = lines word_list
    --     words !! line_number

random_word_action :: IO String
random_word_action = do
    content <- dictionary_action
    n <- random_number_action
    return $ word_from_list n content

-- turn_wrapper :: IO String -> Int -> IO String
-- turn_wrapper word n =
--     do
--         return $ turn word ['-' | x <- word] n

guessgame :: Int -> IO ()
guessgame n = do
    word <- random_word_action
    let hidden = hidden_word word
    print word
    -- return $ turn word hidden n
    -- return $ turn word hidden n
    -- return $ turn word ['-' | x <- word] n
    -- turn_wrapper word n
    -- turn word ['-' | x <- word] n
