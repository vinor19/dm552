module Lib (
 generateRandom,
 isValid,
 isValidAux,
 movesNumbers
 ) where

import System.IO
import Control.Monad
import System.Random
import Data.Maybe (fromJust)
import Text.Read (readMaybe)
import Data.List ((\\),sort, length, intersect)


-- isValidAux is the auxilary function to isValid
-- It checks if the given moves etc. are legal moves and applies them accordingly

isValidAux :: ([String],[(Integer, Integer)],[(Integer,Integer)],Integer) -> [((Integer, Integer),(Integer, Integer),String)] -> IO (String)
isValidAux ((z:zs),(x:xs),(y:ys), p) [] = return (show ((z:zs),(x:xs),(y:ys), p))
isValidAux ((z:zs),[],(y:ys),0) (m:ms) = return $ "NonValid " ++  (show m)
isValidAux ((z:zs),(x:xs),[],1) (m:ms) = return $ "NonValid " ++  (show m)
isValidAux ((z:zs),[],(y:ys),0) _ = return ("(" ++ (show(z:zs)) ++ ",[],"++ (show(y:ys)) ++ ", 0)")
isValidAux ((z:zs),(x:xs),[],1) _ = return ("(" ++ (show(z:zs)) ++ "," ++ (show(x:xs)) ++ ",[]" ++ ", 1)")
isValidAux ((z:zs),(x:xs),(y:ys), p) (m:ms) = do
 if (isValidGameState ((z:zs), (x:xs), (y:ys),p))
  then do 
   let nonValid = if (p == 0) then isValidMove p m (x:xs) [(z:zs)!!0, (z:zs)!!1] else isValidMove p m (y:ys) [(z:zs)!!2, (z:zs)!!3]
   if nonValid
    then do
      (isValidAux (applyMove ((z:zs),(x:xs),(y:ys), p) m) ms)
    else do
     return("NonValid " ++ (show m))
  else do
   return("ParsingError")


-- isValid takes a FilePath and parses it
-- And gives the information to isValidAux if it passes the parsing
isValid :: FilePath -> IO (String)
isValid path = do
 handl <- readFile path
 let contents = lines handl
 let x = parseGameState $ head contents
 if x == Nothing then do
  return "ParsingError"
 else do
  let moves = fmap parseMoves (tail contents)
  if Nothing `elem` moves then do
   return "ParsingError"
  else do
   isValidAux (fromJust x) (map fromJust moves)


generateRandom :: Int -> Int -> IO (String)
generateRandom _ _ = return "Not yet implemented"

movesNumbers :: Int -> String -> IO (String)
movesNumbers _ _ = return "Not yet implemented"

-- Helper functions for isValid


-- getPositionChange returns how the position of a piece should change based on the card given
getPositionChange :: String -> [(Integer,Integer)]
getPositionChange "Rabbit" = [(-1,-1), (1,1), (2,0)]
getPositionChange "Cobra" = [(-1,0), (1,1), (1,-1)]
getPositionChange "Rooster" = [(-1,0), (-1,-1), (1,0), (1,1)]
getPositionChange "Tiger" = [(0,2),(0,-1)]
getPositionChange "Monkey" = [(1,1), (-1,1), (-1,-1), (1,-1)]
getPositionChange "Crab" = [(-2,0), (2, 0), (0,1)]
getPositionChange "Crane" = [(-1,-1),(0,1),(1,-1)]
getPositionChange "Frog" = [(1,-1), (-1,1), (-2,0)]
getPositionChange "Boar" = [(-1,0), (0,1), (1,0)]
getPositionChange "Horse" = [(-1,0), (0,1), (0,-1)]
getPositionChange "Elephant" = [(-1,0), (-1,1), (1,0), (1,1)]
getPositionChange "Ox" = [(1,0), (0,1), (0,-1)]
getPositionChange "Goose" = [(-1,0), (-1,1), (1,0), (1,-1)]
getPositionChange "Dragon" = [(-2,1), (-1,-1), (2,1), (1,-1)]
getPositionChange "Mantis" = [(-1,1), (0,1), (1,1)]
getPositionChange "Eel" = [(-1,1), (-1,-1), (1,0)]

-- newPositions gives the new positions that are possible after a card is applied to the given piece
newPositions _ _ [] = []
newPositions p (a,b) (x:xs)
 | p == 1 = [(a - fst x, b - snd x)] ++ newPositions p (a,b) (xs)
 | p == 0 = [(a + fst x, b + snd x)] ++ newPositions p (a,b) (xs)
 | otherwise = []

-- Checks if it's a valid game state, returns true if it is, false if it isn't
isValidGameState :: ([String],[(Integer, Integer)],[(Integer,Integer)],Integer) -> Bool
isValidGameState ((z:zs),(x:xs),(y:ys),p) = foldl (&&) True [(length (z:zs)) == 5 , (p == 1) || (p == 0) , allUnique (z:zs) , allUnique ((x:xs) ++ (y:ys)) , (0,0) <= (minimum (x:xs)) , (4,4) >= (maximum (x:xs)) , (0,0) <= (minimum (y:ys)) , (4,4) >= (maximum (y:ys)) , (z:zs) == ((z:zs) `intersect` cardList), xs == (sort xs),ys == (sort ys)]

-- Variable with all legal card names
cardList = ["Rabbit", "Cobra", "Rooster", "Tiger", "Monkey", "Crab", "Crane", "Frog", "Boar", "Horse", "Elephant", "Ox", "Goose", "Dragon", "Mantis", "Eel"]

-- Checks if a move is legal, returns true if the moves is valid, false if it is not
isValidMove p ((a,b),(c,d),s) (x:xs) (y:ys) = foldl (&&) True [0<=c, c<=4, 0<=d, d<=4 , elem s (y:ys), elem (c,d) (newPositions p (a,b) ((getPositionChange s))), not (elem (c,d) (x:xs))]

-- Updates the list after a move is performed
updatedList :: [(Integer, Integer)] -> (Integer, Integer) -> (Integer, Integer) -> [(Integer,Integer)]
updatedList (x:xs) (a,b) (c,d) = [if y == (a,b) then (c,d) else y | y <- (x:xs)]

-- Updates the cards so that they are sorted correctly, and the players have the proper cards, returns the list of cards
updateCards :: [[Char]] -> [Char] -> Integer ->  [[Char]]
updateCards z s p= do
 if p == 0
  then do
   let playerCards = [z!!0, z!!1]
   let playerCardsUpdated = sort ((playerCards\\[s]) ++ [z!!4])
   playerCardsUpdated ++ [z!!2,z!!3] ++ [s]
  else do
   let playerCards = [z!!2,z!!3]
   let playerCardsUpdated = sort((playerCards\\[s]) ++ [z!!4])
   [z!!0,z!!1] ++ playerCardsUpdated ++ [s]

-- Applies the move that is being made returns a game state
applyMove :: ([String],[(Integer, Integer)],[(Integer,Integer)],Integer) -> ((Integer, Integer),(Integer, Integer),String) ->([String],[(Integer, Integer)],[(Integer,Integer)],Integer)
applyMove ((z:zs),(x:xs),(y:ys), p) ((a,b),(c,d),s) = do
 if p == 0
  then do
   if (((c,d) == y)||(c,d) == (4,2))
    then ((updateCards (z:zs) s p),(updatedList (x:xs) (a,b) (c,d)),[],1)
   else do
    let (g:gs) = (y:ys)\\[(c,d)]
    let (h:hs) = updatedList (x:xs) (a,b) (c,d)
    let k = updateCards (z:zs) s p
    (k,(h:(sort hs)),(g:(sort gs)),1)
  else do
    if (((c,d) == x)||(c,d) == (0,2))
     then ((updateCards (z:zs) s p),[],(updatedList (y:ys) (a,b) (c,d)),0)
     else do
      let (g:gs) = (x:xs)\\[(c,d)]
      let (h:hs) = updatedList (y:ys) (a,b) (c,d)
      let k = updateCards (z:zs) s p
      (k,(g:(sort gs)),(h:(sort hs)),0)

-- Parses the gamestate line in the file, if it can parse it, then it returns the gamestate, if not it returns Nothing
parseGameState :: String -> Maybe ([String],[(Integer,Integer)],[(Integer,Integer)],Integer)
parseGameState = readMaybe

-- Parses the moves, returns the parsed move if it succeeds, returns Nothing if it can't
parseMoves :: String -> Maybe ((Integer,Integer),(Integer,Integer),String)
parseMoves = readMaybe


-- Function to check if every element of a list is unique
allUnique [] = True
allUnique (x:xs) = not((x `elem` xs)) && (allUnique xs)
