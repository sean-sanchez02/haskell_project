module Main where

import Syntax
import Semantics

-- Example reviews
r1 :: Review
r1 = Review Movie "Inception"
      (Description "Sean" "2025-11-01" "Christopher Nolan")
      (Rating 9 10)
      (Body "A mind-bending sci-fi thriller."
            ["Visuals", "Soundtrack", "Concept"]
            ["Complex plot"])

r2 :: Review
r2 = Review Music "Sempiternal"
      (Description "Sean" "2025-11-02" "Bring Me The Horizon")
      (Rating 9.5 10)
      (Body "A metalcore album that dives into the experiences of its lead singer."
            ["Production", "Vocals", "Story"]
            ["Lyrics in some parts could be better"])

r3 :: Review
r3 = Review Movie "Interstellar"
      (Description "Alex" "2024-05-20" "Christopher Nolan")
      (Rating 9.7 10)
      (Body "A sprawling space epic with emotional core."
            ["Scope", "Score", "Visuals"]
            ["Pacing issues"])

-- example program 1: add two reviews, print all, stats
prog1 :: Program
prog1 = BeginEnd
  [ Add r1
  , Add r2
  , PrintAll
  , Stats AverageRating
  ]

-- example program 2: add, update rating, print title and type, stats highest
prog2 :: Program
prog2 = BeginEnd
  [ Add r3
  , PrintAll
  , UpdateRating "Interstellar" 9.9
  , PrintByTitle "Interstellar"
  , PrintByType Movie
  , Stats HighestRated
  ]

-- example program 3: edit and delete
r1Edited :: Review
r1Edited = Review Movie "Inception"
            (Description "Sean" "2025-11-01" "Christopher Nolan")
            (Rating 9.2 10)
            (Body "A mind-bending sci-fi thriller (revisited)."
                  ["Visuals","Soundtrack","Concept"]
                  ["Complex plot"])

prog3 :: Program
prog3 = BeginEnd
  [ Add r1
  , Add r2
  , Edit "Inception" r1Edited
  , PrintByTitle "Inception"
  , Delete "Sempiternal"
  , PrintAll
  , Stats CountByType
  ]

-- example program 4: filter and reviewer frequency
prog4 :: Program
prog4 = BeginEnd
  [ Add r1
  , Add r2
  , Add r3
  , FilterByReviewer "Sean"
  , Stats ReviewerFrequency
  ]

main :: IO ()
main = do
  putStrLn "== Running prog1 =="
  res1 <- evaluateP prog1
  print res1

  putStrLn "\n== Running prog2 =="
  res2 <- evaluateP prog2
  print res2

  putStrLn "\n== Running prog3 =="
  res3 <- evaluateP prog3
  print res3

  putStrLn "\n== Running prog4 =="
  res4 <- evaluateP prog4
  print res4
