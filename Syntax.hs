module Syntax where

{-  -- Review Language
-- Context Free Grammar
<program>       -> begin <commands> end
<commands>      -> [<command>]
<command>       -> add <review>                   --adds a new review
                | edit <title> with <review>      --looks for review by title and then replaces it with new review
                | delete <title>                  --deletes a review
                | print all                       --prints all reviews
                | print type <itemtype>           --prints reviw by type
                | print title <title>             --prints review by title
                | filter reviewer <string>        --filters reviews by the reviewer
                | update rating <title> <float>   --updates numerical rating by title

<review>        -> review <itemtype> ":" <title> "{" <description> <rating> <body> "}"
<itemtype>      -> movie | music | tv | book
<title>         -> String
<description>   -> reviewer: String date: String creator: String
<rating>        -> rating: Float "/" Float
<body>          -> summary: String pros: [String] cons: [String]
-}

-- ABSTRACT SYNTAX

data Program = BeginEnd [Command]
  deriving Eq

type Commands = [Command]

data Command
  = Add Review
  | Edit Title Review
  | Delete Title
  | PrintAll
  | PrintByType ItemType
  | PrintByTitle Title
  | FilterByReviewer String
  | UpdateRating Title Float
  deriving Eq

data Review = Review ItemType Title Description Rating Body
  deriving Eq

data ItemType = Movie | Music | Tv | Book
  deriving (Eq, Ord)

type Title = String

data Description = Description
  { reviewer :: String
  , date     :: String
  , creator  :: String
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

showCommands :: Commands -> String
showCommands [] = ""
showCommands (c:cs) = show c ++ showCommands cs

instance Show Program where
  show (BeginEnd cmds) = "begin\n" ++ showCommands cmds ++ "end\n"

instance Show Command where
  show (Add r)             = "add " ++ show r
  show (Edit t r)          = "edit " ++ show t ++ " with " ++ show r
  show (Delete t)          = "delete " ++ show t ++ "\n"
  show PrintAll            = "print all\n"
  show (PrintByType it)    = "print type " ++ show it ++ "\n"
  show (PrintByTitle t)    = "print title " ++ show t ++ "\n"
  show (FilterByReviewer s)= "filter reviewer " ++ show s ++ "\n"
  show (UpdateRating t v)  = "update rating " ++ show t ++ " " ++ show v ++ "\n"

instance Show ItemType where
  show Movie = "movie"
  show Music = "music"
  show Tv    = "tv"
  show Book  = "book"

instance Show Review where
  show (Review it title desc rating body) =
    "review " ++ show it ++ ": " ++ show title ++ " {\n" ++
    show desc ++ show rating ++ show body ++ "}\n"

instance Show Description where
  show (Description rr d c) =
    "description {\n" ++
    "  reviewer: " ++ show rr ++ "\n" ++
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
