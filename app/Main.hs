{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BlockArguments #-}

module Main where

import Control.Monad (unless)
import Data.Char     (isSpace, toUpper)
import System.IO     (hFlush, stdout)
import System.Random (randomRIO)
import Text.Printf   (printf)

maxAttempt, lowerBound, higherBound :: Integer
maxAttempt = 10
lowerBound = 0
higherBound = 100

getGuess :: IO (Maybe Integer)
getGuess = go . reads <$> getLine
  where
    go = \case ((n, _) : _) -> Just n; _ -> Nothing

processAttempt :: Integer -> Integer -> IO Bool
processAttempt answer currentAttempt = do
    printf "[%d/%d]: " currentAttempt maxAttempt
    hFlush stdout
    maybeGuess <- getGuess

    case maybeGuess of
        Nothing         -> processAttempt    answer currentAttempt
        Just guessedNum -> processValidGuess answer guessedNum

processValidGuess :: Integer -> Integer -> IO Bool
processValidGuess answer guessedNum = do
    case compare guessedNum answer of
        EQ -> do
            printf "Yes! It was %d! You won!\n" answer
            pure True
        LT -> do
            putStrLn "No! The answer is ᵇᶦᵍᵍᵉʳ!"
            pure False
        GT -> do
            putStrLn "No! The answer is ₛₘₐₗₗₑᵣ!"
            pure False

guessLoop :: Integer -> Integer -> IO ()
guessLoop answer currentAttempt
    | currentAttempt > maxAttempt =
        printf "Sorry, you ran out of attempts! The answer was %d.\n" answer
    | otherwise = do
        won <- processAttempt answer currentAttempt
        unless won $ guessLoop answer (currentAttempt + 1)

playGame :: IO ()
playGame = do
    answer <- randomRIO @Integer (lowerBound, higherBound)
    printf "I generated a Random Number from %d to %d.\n" lowerBound higherBound
    printf "You have %d attempts to guess it.\n" maxAttempt
    guessLoop answer 1
    askPlayAgain

askPlayAgain :: IO ()
askPlayAgain = do
    printf "Try again? [Y/n/x/t/q] "
    hFlush stdout
    input <- map toUpper . strip <$> getLine
    unless (input `elem` ["NO", "N", "Q", "TIDAK", "T", "X"]) playGame

strip :: String -> String
strip = f . f where f = reverse . dropWhile isSpace

main :: IO ()
main = playGame
