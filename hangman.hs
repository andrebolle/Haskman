
-- An implementation of Hangman in Haskell based around the idea that the entire state-changing
-- mechanism can be represented as a Monoid.

module Main where

import Data.List (intersperse)
import Data.Maybe (fromMaybe)
import Data.Monoid
import Control.Monad

import System.Random (getStdRandom, randomR)

-- Game state
data GameState = Finished String |
                 InPlay [Maybe Char] (Char -> GameState)

-- Display the state
instance Show GameState where
  -- fromMaybe default maybe = case maybe of Nothing -> default; Just value  -> value
  -- from a list of maybe chars to a string
  -- show (InPlay letters _)  = "InPlay: " ++ (intersperse ' ' $ map (fromMaybe '_') letters)
  show (InPlay letters _)  = "The word so far: " ++ intersperse ' ' (map (fromMaybe '_') letters)
  -- show (Play letters _)  = show letters

  show (Finished string) = show $ "Well done. The word is " ++ string

instance Monoid GameState where
  mempty = Finished ""   -- The identity element
  mappend = merge       -- The associative binary function. a -> a -> a

-- Filter the right kinds of words - remove words with uppercase letters and apostrpohes
wordFilter :: String -> Bool
wordFilter w = length w > 4 && length w < 7 && all (`notElem` '\'':['A'..'Z']) w

getWord :: IO String
getWord = do 
  wordFile <- readFile "words"
                 -- no proper nouns (i.e. words with uppercase letters) and
                 -- no words with apostrophes
  --let wordList = filter (all (`notElem` '\'':['A'..'Z']))
                   -- words with more than 5 letters
                   --(filter (\x -> length x > 4) (words wordFile)) 
  let wordList = filter wordFilter (words wordFile)
  -- Pick a random number ...
  rand <- getStdRandom(randomR (0, length wordList - 1))
  -- ... and use that number to pick a word
  return (wordList !! rand)

-- Encapsulates the idea of an initial state, a char to match against and a function to get to the next state giving a guess
initialState :: Char -> GameState
initialState actualChar = InPlay [Nothing] (\guessChar -> if guessChar == actualChar then Finished [actualChar] else initialState actualChar)

main = do
  -- word :: [Char]
  word <- getWord
  -- FYI: mconcat = foldr merge (Finished "")
  gameLoop (mconcat (map initialState word))

gameLoop :: GameState -> IO ()

-- Game over
gameLoop (Finished s) = putStrLn $ "You won. The answer is " ++ s
-- Get a new guess and apply it to "f" to get the next state
gameLoop this@(InPlay _ f) = do
  putStrLn $ "The word so far: " ++ show this
  putStrLn "Guess a letter."
  input <- getLine
  let guess = head input
  gameLoop (f guess)

-- Hard to think of a name for this function. "Merge" seems better than "Append"
merge :: GameState -> GameState -> GameState

 -- Two "Finished" states means its really is "Game Over"
merge (Finished string1) (Finished string2)   = Finished (string1 ++ string2)

-- otherwise we are still in play
-- i.e. pairs involving InPlay states result in InPlay states

merge (Finished string) (InPlay maybeChars f) = InPlay (map Just string ++ maybeChars)
                                                       -- (\guess -> merge (Finished string) (f guess))
                                                       (merge (Finished string) . f)

merge (InPlay maybeChars f) (Finished string) = InPlay (maybeChars ++ map Just string)
                                                       -- (\guess -> merge (f guess) (Finished string))
                                                       (flip merge (Finished string) . f)

merge (InPlay xs1 f1) (InPlay xs2 f2)         = InPlay (xs1 ++ xs2)
                                                       -- (\guess -> merge (f1 guess) (f2 guess))
                                                       (liftM2 merge f1 f2)
