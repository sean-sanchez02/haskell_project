module Main where

import Syntax
import Semantics

-- Example reviews
r1 :: Review
r1 = Review Movie "Inception"
      (Description "Sean" "11/01/2025" "Christopher Nolan")
      (Rating 9 10)
      (Body "A mind-bending sci-fi thriller."
            ["Visuals", "Soundtrack", "Concept"]
            ["Complex plot"])

r2 :: Review
r2 = Review Music "Sempiternal"
      (Description "Sean" "11/02/2025" "Bring Me The Horizon")
      (Rating 9.5 10)
      (Body "A metalcore album that dives into the experiences of its lead singer."
            ["Production","Vocals","Story"]
            ["Lyrics in some parts could be better"])

r3 :: Review
r3 = Review Movie "Interstellar"
      (Description "Alex" "12/11/2025" "Christopher Nolan")
      (Rating 9.7 10)
      (Body "A sprawling space epic with emotional core."
            ["Scope","Score","Visuals"]
            ["Pacing issues"])

-- Program 1: add + print
prog1 :: Program
prog1 = BeginEnd
  [ Add r1
  , Add r2
  , PrintAll
  ]

-- Program 2: update + print
prog2 :: Program
prog2 = BeginEnd
  [ Add r3
  , UpdateRating "Interstellar" 9.9
  , PrintByTitle "Interstellar"
  ]

-- Program 3: edit + delete + print
r1Edited :: Review
r1Edited = Review Movie "Inception"
            (Description "Sean" "12/11/2025" "Christopher Nolan")
            (Rating 9.2 10)
            (Body "A mind-bending sci-fi thriller (revisited)."
                  ["Visuals","Soundtrack","Concept"]
                  ["Complex plot"])

prog3 :: Program
prog3 = BeginEnd
  [ Add r1
  , Add r2
  , Edit "Inception" r1Edited
  , Delete "Sempiternal"
  , PrintAll
  ]

-- Program 4: filter reviewer
prog4 :: Program
prog4 = BeginEnd
  [ Add r1
  , Add r2
  , Add r3
  , FilterByReviewer "Sean"
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
