module Syntax where

{-  -- Review Language
-- Context-Free Grammar
<program>        -> begin <reviews> end
<reviews>        -> [<review>]
<review>         -> review <itemtype> ":" <title> "{" <description> <rating> <body> "}"
<itemtype>       -> movie | music | tv | book
<title>          -> String
<description>    -> reviewer: String date: String creator: String
<rating>         -> rating: Float "/" Float
<body>           -> summary: String pros: [String] cons: [String]

-- Environment / Evaluation not yet included
-}

-- ABSTRACT SYNTAX

data Program = BeginEnd Reviews

type Reviews = [Review]

data Review = Review ItemType Title Description Rating Body
  deriving Eq

data ItemType = Movie | Music | Tv | Book
  deriving Eq

type Title = String

data Description = Description
  { reviewer  :: String
  , date    :: String
  , creator :: String
  } deriving Eq

data Rating = Rating
  { value :: Float
  , scale :: Float
  } deriving Eq

data Body = Body
  { summary :: String
  , pros    :: [String]
  , cons    :: [String]
  } deriving Eq


-- SHOW INSTANCES

showReviews :: Reviews -> String
showReviews [] = ""
showReviews (r:rs) = show r ++ showReviews rs

instance Show Program where
  show (BeginEnd rs) = "begin\n" ++ showReviews rs ++ "end\n"

instance Show ItemType where
  show Movie = "movie"
  show Music = "music"
  show Tv = "tv"
  show Book = "book"

instance Show Description where
  show (Description rr d c) =
    "description {\n" ++
    "  reviewer: " ++ show  rr ++ "\n" ++
    "  date: " ++ show d ++ "\n" ++
    "  creator: " ++ show c ++ "\n" ++
    "}\n"

instance Show Rating where
  show (Rating v s) = "rating: " ++ show v ++ "/" ++ show s ++ "\n"

instance Show Body where
  show (Body sum ps cs) =
    "body {\n" ++
    "  summary: " ++ show sum ++ "\n" ++
    "  pros: " ++ show ps ++ "\n" ++
    "  cons: " ++ show cs ++ "\n" ++
    "}\n"

instance Show Review where
  show (Review it title description rating body) =
    "review " ++ show it ++ ": " ++ show title ++ " {\n" ++
    show description ++ show rating ++ show body ++ "}\n"
