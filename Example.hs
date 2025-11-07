import Syntax

{- 
begin
  review movie: "Inception" {
    description {
      reviewer: "Sean"
      date: "11/01/2025"
      creator: "Christopher Nolan"
    }
    rating: 9/10
    body {
      summary: "A mind-bending sci-fi thriller."
      pros: ["Visuals", "Soundtrack", "Concept"]
      cons: ["Complex plot"]
    }
  }

  review music: "Sempiternal" {
    description {
      reviewer: "Sean"
      date: "11/02/2025"
      creator: "Bring Me The Horizon"
    }
    rating: 9.5/10
    body {
      summary: "A metalcore album that dives into the experiences of its lead singer."
      pros: ["Production", "Vocals", "Story"]
      cons: ["Lyrics in some parts could be better"]
    }
  }
end
-}

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
      (Body "A metalcore album that dives into the experience of its lead singer."
            ["Production", "Vocals", "Story"]
            ["Lyrics in some parts could be better"])

p1 :: Program
p1 = BeginEnd [r1, r2]

main :: IO()
main = do
  putStrLn "Displaying review program:"
  print p1
