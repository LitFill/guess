module Main where

import Data.Char (toUpper)
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
  guess' answer 1
 where
  guess' ans at =
    if at > maxAttempt
      then do
        printf "Sorry you ran out of attempts! The answer was %d.\n" ans
        more
      else do
        printf "[%d/%d]: " at maxAttempt
        flushAll
        g <- getLine
        case readMaybe @Integer g of
          Nothing -> do
            putStrLn "Masukkan angka"
            guess' ans at
          Just n ->
            case compare n ans of
              EQ -> do printf "Yes! It was %d! You won!\n" ans; more
              LT -> do printf "No! The answer is ᵇᶦᵍᵍᵉʳ!\n"; guess' ans $ at + 1
              GT -> do printf "No! The answer is ₛₘₐₗₗₑᵣ!\n"; guess' ans $ at + 1

more :: IO ()
more = do
  printf "Try again? [Y/n] "
  flushAll
  gl <- getLine
  case map toUpper gl of
    "NO" -> pure ()
    "N" -> pure ()
    _ -> guess

main :: IO ()
main = do
  printf "I generated a Random Number from %d to %d.\n" lowerBound higherBound
  printf "You have %d attempts to guess it.\n" maxAttempt
  guess
