-- https://www.futurelearn.com/courses/functional-programming-haskell/1/steps/108393
-- custom data types

data PersonRecord  = MkPersonRecord {
    name :: String,
    address :: Address,
    id :: Integer,
    labels :: [Label]
} deriving (Show)

data Address = MkAddress {
    line1 :: String,
    number :: Integer,
    street :: String,
    town :: String,
    postcode :: String
} deriving (Show)

data Label = Green | Red | Blue | Yellow deriving (Show)

rec1 = MkPersonRecord
    "Wim Vanderbauwhede"
    (MkAddress "School of Computing Science" 17 "Lilybank Gdns" "Glasgow" "G12 8QQ")
    557188
    [Green, Red]

rec2 = MkPersonRecord
    "Jeremy Singer"
    (MkAddress "School of Computing Science" 17 "Lilybank Gdns" "Glasgow" "G12 8QQ")
    42
    [Blue, Yellow]

main = putStrLn $ show [rec1,rec2]


module ShowParser ( parseShow ) where

    parseShow :: String -> String
    parseShow = run_parser showParser

    showParser :: Parser String

    run_parser :: Parser a -> String -> a
    run_parser p str =  case parse p "" str of
        Left err -> error $ "parse error at " ++ (show err)
        Right val  -> val
