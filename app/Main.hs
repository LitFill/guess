module Main where

import Control.Monad (foldM, unless)
import Data.Char (isSpace, toUpper)
import GHC.GHCi.Helpers (flushAll)
import System.Random (randomRIO)
import Text.Printf (printf)
import Text.Read (readMaybe)

maxAttempt, lowerBound, higherBound :: Integer
maxAttempt = 10
lowerBound = 0
higherBound = 100

guess :: IO ()
guess = do
  answer <- randomRIO @Integer (lowerBound, higherBound)
  let attemptAction at = do
        printf "[%d/%d]: " at maxAttempt
        flushAll
        g <- fmap (`compare` answer) . readMaybe @Integer <$> getLine
        case g of
          Just EQ -> printf "Yes! It was %d! You won!\n" answer >> pure True
          Just LT -> putStrLn "No! The answer is ᵇᶦᵍᵍᵉʳ!" >> pure False
          Just GT -> putStrLn "No! The answer is ₛₘₐₗₗₑᵣ!" >> pure False
          Nothing -> putStrLn "This is not even a number, come on ._." >> pure False
  let check acc at = if acc then pure True else attemptAction at
  won <- foldM check False [1 .. maxAttempt]
  unless won $ printf "Sorry, you ran out of attempts! The answer was %d.\n" answer
  more

more :: IO ()
more = do
  printf "Try again? [Y/n/x/t/q] "
  flushAll
  gl <- map toUpper . strip <$> getLine
  unless (gl `elem` ["NO", "N", "Q", "TIDAK", "T", "X"]) guess

strip :: String -> String
strip = f . f where f = reverse . dropWhile isSpace

main :: IO ()
main = do
  printf "I generated a Random Number from %d to %d.\n" lowerBound higherBound
  printf "You have %d attempts to guess it.\n" maxAttempt
  guess
