--  File     : musicmind.hs
--  Author   : Hengfeng Li
--  Purpose  : a MusicMind game
module Musicmind 
(initialGuess,
 nextGuess,
 GameState
) where

import qualified Data.Set as Set
import Data.List

-- The GameState maintain the list of possible answers
-- in which each element is a Set type.
type GameState = [Set.Set [Char]]

-- Generate all 1330 possible answers in the first stage
-- by using the List Conprehension to select three element
-- from a collection of all pitches.
allChord :: GameState
allChord = Set.toList $ Set.fromList [ Set.fromList [a,b,c] |
    a <- allPitches, 
    b <- [x | x <- allPitches, x /= a],
    c <- [x | x <- allPitches, x /= a, x /= b]]
    where allPitches = [ x:[y] | x <- "ABCDEFG", y <- "123"]

-- Generate a list which select one element from each patterns.
firstGroups :: [Set.Set String]
firstGroups = map snd $ nubBy (\x y -> (fst x) == (fst y)) allTuples
    where allTuples = map getPattern allChord

-- Compute the pattern to reduce the 1330 possible answers 
-- when deciding the first guess. As we known, there are 
-- 6 patterns at the first stage.
getPattern :: Set.Set String -> ([Int], Set.Set String)
getPattern x = (map (length . nub) $ transpose $ Set.toList x, x)

-- Compute the first guess which one will lead to the 
-- maximum number of possible answers leaving.
firstGuess :: GameState -> ([String],Int)
firstGuess gs
    = foldr (\x acc -> 
            let num = maxSameOutput $ sameOutputs (Set.toList x) (delete x allChord)
            in 
            if num < snd acc then ((Set.toList x),num) else acc
            ) ([""],length allChord) gs

-- Initialize the first guess and game state.
initialGuess :: ([String], GameState)
initialGuess = (guess, initialGameState)
    where guess       = fst $ firstGuess firstGroups
          initialGameState = delete (Set.fromList guess) allChord

-- Delete all the inconsistent possible answers in game state.
delInconsistent :: ([String],GameState)->(Int,Int,Int) -> GameState
delInconsistent (prev, gamestate) answer 
    = filter (\x -> response prev (Set.toList x) == answer) gamestate

-- Generate the next guess according to the previous guess, game state
-- and answer.
nextGuess :: ([String],GameState)->(Int,Int,Int)->([String],GameState)
nextGuess (a, xs) answer = (x, zs)
    where ys = delInconsistent (a,xs) answer
          x  = fst $ maxLeavingGuess ys
          zs = delete (Set.fromList x) ys 

-- Get the guess that the maximum number of 
-- possible answers will leave.
maxLeavingGuess :: GameState -> ([String],Int)
maxLeavingGuess gs
    | length gs == 1 = (Set.toList $ head gs, 0)
    | otherwise      = foldr (\x acc -> 
            let num = maxSameOutput $ sameOutputs (Set.toList x) (delete x gs)
            in 
            if num < snd acc then ((Set.toList x),num) else acc
            ) ([""],length allChord) gs

-- Get the maximum number of the same outputs.
maxSameOutput :: [Int] -> Int
maxSameOutput = foldr (\x acc -> if x > acc then x else acc) 0

-- Compute the number of possible answers which has
-- the same output answer in the specific guess.
sameOutputs :: [String] -> GameState -> [Int]
sameOutputs guess gs = map length $ group $ sort $ map (response guess . Set.toList) gs

-- Compute the answer between target and guess.
response :: [String] -> [String] -> (Int,Int,Int)
response target guess = (mPitches,mNotes,mOctave)
    where   targetSet = Set.fromList target
            guessSet  = Set.fromList guess
            intersection = Set.intersection targetSet guessSet
            mPitches = Set.size intersection
            diffTarget = Set.difference targetSet intersection
            diffGuess  = Set.difference guessSet intersection
            targetNotes = nub $ map head $ Set.toList diffTarget 
            guessNotes = nub $ map head $ Set.toList diffGuess
            mNotes = foldr (\x acc -> 
                    if elem x guessNotes 
                    then acc + 1 else acc) 0 targetNotes
            targetOctaves = sort $ map tail $ Set.toList diffTarget
            guessOctaves = sort $ map tail $ Set.toList diffGuess
            mOctave = computeOctaves targetOctaves guessOctaves

-- Compute how many octaves are same.
computeOctaves :: [String] -> [String] -> Int
computeOctaves _ [] = 0
computeOctaves [] _ = 0
computeOctaves (x:xs) ys 
    | elem x ys = 1 + computeOctaves xs (delete x ys)
    | otherwise = computeOctaves xs ys

