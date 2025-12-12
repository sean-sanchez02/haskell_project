module Semantics where

import Syntax
import Data.List (find)

-- Environment = list of reviews
type Env = [Review]

data Result = Valid Env | Invalid String
  deriving Show

-- Top-level evaluator
evaluateP :: Program -> IO Result
evaluateP (BeginEnd cmds) = evaluateCommands cmds []

-- Run through command list
evaluateCommands :: [Command] -> Env -> IO Result
evaluateCommands [] env = return (Valid env)
evaluateCommands (c:cs) env = do
  r <- evaluateCommand c env
  case r of
    Left err -> return (Invalid err)
    Right env' -> evaluateCommands cs env'

-- Evaluate a single command
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
  return (Right env)

evaluateCommand (PrintByType it) env = do
  putStrLn (formatReviews (filter (\(Review i _ _ _ _) -> i == it) env))
  return (Right env)

evaluateCommand (PrintByTitle t) env =
  case find (\rv -> titleEq rv t) env of
    Nothing -> return $ Left "PrintByTitle: review not found"
    Just rv -> do putStrLn (show rv); return (Right env)

evaluateCommand (FilterByReviewer rname) env = do
  let filtered = filter (\(Review _ _ (Description rr _ _) _ _) -> rr == rname) env
  putStrLn (formatReviews filtered)
  return (Right env)

evaluateCommand (UpdateRating t newVal) env =
  if any (\rv -> titleEq rv t) env
    then return $ Right (map (updateIfMatches t newVal) env)
    else return $ Left "UpdateRating: review not found"

-- Helpers

titleEq :: Review -> Title -> Bool
titleEq (Review _ t _ _ _) titleStr = t == titleStr

sameTitle :: Review -> Review -> Bool
sameTitle (Review _ t1 _ _ _) (Review _ t2 _ _ _) = t1 == t2

updateIfMatches :: Title -> Float -> Review -> Review
updateIfMatches t newVal rv@(Review it ttl desc (Rating _ s) body)
  | ttl == t  = Review it ttl desc (Rating newVal s) body
  | otherwise = rv

formatProgram :: Env -> String
formatProgram env = "begin\n" ++ formatReviews env ++ "end\n"

formatReviews :: Env -> String
formatReviews [] = ""
formatReviews (r:rs) = show r ++ formatReviews rs

