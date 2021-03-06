import System.Random


check :: String -> String -> Char -> (Bool, String)
check word display c
    = (c `elem` word, [if x==c
        then c
        else y | (x,y) <- zip word display])


turn_action :: String -> String -> Int -> IO ()
turn_action word display n = do
    if n==0
    then putStrLn ("You lose (it was " ++ word ++ ")")
    else if word==display
        then putStrLn ("You win! (it was " ++ word ++ ")")
        else make_guess_action word display n


make_guess_action :: String -> String -> Int -> IO ()
make_guess_action word display n = do
    putStrLn (display ++ "  " ++ take n (repeat '*'))
    putStr "  Enter your guess: "
    q <- getLine
    let (correct, display') = check word display (q!!0)
    let n' = if correct then n else n-1
    turn_action word display' n'


hidden_word :: String -> String
hidden_word word = ['-' | x <- word]


starman :: String -> Int -> IO ()
starman word n = turn_action word (hidden_word word) n

--

dictionary_action :: IO String
dictionary_action = readFile "/usr/share/dict/words"

random_number_action :: Int -> IO Int
random_number_action end = randomRIO (0, end)

word_from_list :: Int -> String -> String
word_from_list line_number word_list = (lines word_list) !! line_number

random_word_action :: IO String
random_word_action = do
    -- bind the results of actions to a name, within the IO monad
    content <- dictionary_action
    let l = length (lines content)
    n <- random_number_action l
    let word = word_from_list n content
    -- `return` 'lifts' the result of a pure function into the IO monad
    return $ word


guessgame :: Int -> IO ()
guessgame n = do
    word <- random_word_action
    turn_action word (hidden_word word) n

main = guessgame 10
