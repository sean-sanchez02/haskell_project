module Semantics where

import Syntax
import Data.List (find, partition, sortBy, groupBy)
import Data.Function (on)
import Data.Maybe (isJust, fromJust)
import Control.Monad (foldM)
import qualified Data.Map.Strict as Map

-- Environment: list of reviews (freshest at head)
type Env = [Review]

data Result = Valid Env | Invalid String
  deriving Show

-- ENTRY POINT
evaluateP :: Program -> IO Result
evaluateP (BeginEnd cmds) = evaluateCommands cmds []

-- Evaluate a sequence of commands with an environment (IO because of printing)
evaluateCommands :: [Command] -> Env -> IO Result
evaluateCommands [] env = return (Valid env)
evaluateCommands (c:cs) env = do
  r <- evaluateCommand c env
  case r of
    Left err -> return (Invalid err)
    Right env' -> evaluateCommands cs env'

-- Evaluate a single command (returns Either error updated-env) in IO
evaluateCommand :: Command -> Env -> IO (Either String Env)
evaluateCommand (Add r) env =
  if any (sameTitle r) env
    then return $ Left "Add: review with same title already exists"
    else return $ Right (r : env)

evaluateCommand (Edit t newR) env =
  if any (\rv -> titleEq rv t) env
    then return $ Right (map (\rv -> if titleEq rv t then newR else rv) env)
    else return $ Left "Edit: review not found"

evaluateCommand (Delete t) env =
  if any (\rv -> titleEq rv t) env
    then return $ Right (filter (not . (`titleEq` t)) env)
    else return $ Left "Delete: review not found"

evaluateCommand PrintAll env = do
  putStrLn (formatProgram env)
  return $ Right env

evaluateCommand (PrintByType it) env = do
  putStrLn (formatReviews (filter (\(Review i _ _ _ _) -> i == it) env))
  return $ Right env

evaluateCommand (PrintByTitle t) env =
  case find (\rv -> titleEq rv t) env of
    Nothing -> return $ Left "PrintByTitle: review not found"
    Just rv -> do putStrLn (show rv); return $ Right env

evaluateCommand (FilterByReviewer rname) env = do
  let filtered = filter (\(Review _ _ (Description rev _ _) _ _) -> rev == rname) env
  putStrLn (formatReviews filtered)
  -- Do not modify environment by default; just display filtered results
  return $ Right env

evaluateCommand (UpdateRating t newVal) env =
  if any (\rv -> titleEq rv t) env
    then let env' = map (updateIfMatches t newVal) env
         in return $ Right env'
    else return $ Left "UpdateRating: review not found"

evaluateCommand (Stats sk) env = do
  putStrLn (formatStat sk env)
  return $ Right env

-- Helpers --------------------------------------------------------

-- Title equality helper (case-sensitive)
titleEq :: Review -> Title -> Bool
titleEq (Review _ t _ _ _) titleStr = t == titleStr

-- sameTitle comparing two reviews
sameTitle :: Review -> Review -> Bool
sameTitle (Review _ t1 _ _ _) (Review _ t2 _ _ _) = t1 == t2

-- update rating helper (preserves scale)
updateIfMatches :: Title -> Float -> Review -> Review
updateIfMatches t newVal rv@(Review it ttl desc (Rating _ s) body)
  | ttl == t  = Review it ttl desc (Rating newVal s) body
  | otherwise = rv

-- format program (wrap reviews in begin/end)
formatProgram :: Env -> String
formatProgram env = "begin\n" ++ formatReviews env ++ "end\n"

formatReviews :: Env -> String
formatReviews [] = ""
formatReviews (r:rs) = show r ++ formatReviews rs

-- Stats ----------------------------------------------------------

formatStat :: StatKind -> Env -> String
formatStat AverageRating env =
  let vals = [v | Review _ _ _ (Rating v _) _ <- env]
  in if null vals
       then "Average rating: N/A"
       else "Average rating: " ++ show (sum vals / fromIntegral (length vals))

formatStat HighestRated env =
  if null env then "Highest rated: N/A"
  else
    let cmp (Review _ _ _ (Rating v1 _) _) (Review _ _ _ (Rating v2 _) _) = compare v2 v1
        sorted = sortBy cmp env
        Review _ t _ (Rating v _) _ = head sorted
    in "Highest rated: " ++ show t ++ " (" ++ show v ++ ")"

formatStat CountByType env =
  let counts = foldr (\(Review it _ _ _ _) acc -> Map.insertWith (+) it 1 acc) Map.empty env
      pairs = Map.toList counts
      showPair (it, n) = show it ++ ": " ++ show n
  in if null pairs then "Counts by type: none" else "Counts by type: " ++ unwords (map showPair pairs)

formatStat ReviewerFrequency env =
  let revs = map (\(Review _ _ (Description r _ _) _ _) -> r) env
      grouped = Map.toList $ foldr (\k m -> Map.insertWith (+) k 1 m) Map.empty revs
      showPair (r, n) = r ++ ": " ++ show n
  in if null grouped then "Reviewer frequency: none" else "Reviewer frequency: " ++ unwords (map showPair grouped)

-- End of Semantics.hs
