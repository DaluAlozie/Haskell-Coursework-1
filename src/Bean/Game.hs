module Bean.Game where

--------------------------------------------------------------------------------
-- This file should contain your complete work for the first coursework of 
-- CS141 Functional Programming.
-- 
-- USER ID: 
--
-- Before starting on this file, ensure that you have read the specification IN
-- ITS ENTIRETY and that you understand everything that is required from a good
-- solution.
--------------------------------------------------------------------------------

import Bean.Types( PieceType(..), Player(..), Piece(..), Coord, Board )
import Data.List (intercalate)
import Data.Maybe ( fromJust )


{-| 
  Ex. 1: Write an expression representing the starting position for the game.


    [JUSTIFICATION]

    In this case i chose to hard code the board rather than use list comprehension 
    as the list is relatively small and using list comprehension would have
    resulted in more complicated code and ran slower
  -}
startingPos :: Board
startingPos = [
  [Empty,Red Cow, Red Cow, Empty],
  [Red Bean, Red Bean, Red Bean, Red Bean],
  [Blue Bean, Blue Bean, Blue Bean, Blue Bean],
  [Empty,Blue Cow, Blue Cow, Empty]
  ]


{-|
  Ex. 2: Compute the balance of material, defined as difference between the 
  number of Red beans and the number of Blue beans. Make use of explicit 
  recursion and pattern matching in your definition.

  
  [JUSTIFICATION]

  The balance function basically works by calculating the balance on each row and adding
  all of those balances together 

  I created a general countIF function that counts the number elements in a list that meets 
  a condition, making use of foldr. I could have instead created a more specific function 
  that counted the number of red and blue pieces in the same iteration, so it wouldnt have to 
  iterate a row twice to count the red and blue beans. However, the countIF function is useful
  proves very useful in other functions, and cuts down on repeted code massively 

-}

-- Counts the number of elements in a list that meets a certain condition
countIF :: (a -> Bool) -> ([a] -> Int) 
countIF f  = foldr g 0 
  where g x c = if f x then c + 1 else c


balance :: Board -> Int
balance [] = 0
balance (x:xs) = red - blue + balance xs 
  where 
    blue = countIF (== Blue Bean) x  -- Counts the number of blue beans in a row
    red = countIF (== Red Bean) x    -- Counts the number of red beans in a row


{-| 
  Ex. 3: Implement a 'Show' instance for 'Board'. 
  The rendered version should be exactly as it appears in the specification.
  
  Also implement an 'Eq' instance for 'Piece' and for 'PieceType'.
  You should NOT derive these automatically.

  [JUSTIFICATION]

  Show Instance 

  This was just a case of using map on each row to map each piece into their string 
  representation and then concatinating the row, all by using concatMap, and then joining each 
  row with \n using the intercalate function. This method seemed fairly simple and easy 
  to understand.
  

  Eq Instances

  For i just defined how the (==) operator would work according to the specification. 
  However i just defined as much as i needed to and used _ == _  to define the rest,
  this cuts down code, and also enures that no mistakes are made in the definitions,
  as an mistake is more likely if I defined every possible combination. Its also important 
  to note that the Eq instance for PieceType is required for the (==) to operate properly in
  my Eq instance of Piece

-}
instance {-# OVERLAPS #-} Show Board where
  show :: Board -> String
  show a = intercalate "\n" strBoard 
    where 
      strBoard = map (concatMap show) a  -- A list of lists of one concatinated string e.g [["CBC"]]

instance Eq PieceType where
  (==) :: PieceType -> PieceType -> Bool
  Cow == Cow     = True
  Bean == Bean   = True 
  _ == _         = False

instance Eq Piece where
  (==) :: Piece -> Piece -> Bool

  ( Red a ) == Red b      = a == b 
  ( Blue a ) == Blue b    = a == b 
  Empty == Empty          = True
  _ == _                  = False


{-| 
  Ex. 4: Implement a function that gets a piece at a coordinate. 
  If the location is off the board, return Nothing, otherwise return the 
  appropriate piece (or Empty), wrapped in a Just.

  [JUSTIFICATION]
  
  This was just a case of getting the correct row using using the !! operator and then
  getting the correct element in the row again using the !! operator, with the column 
  and row of the piece being defined by the x and y values of the Coord arguement respectively
-}

getPiece :: Board -> Coord -> Maybe Piece
getPiece brd (x,y) = 
  if x<0 || x>3 || y<0 || y>3 then Nothing   --if the courdinates are out of bound 
  else Just (row !! x)     -- return the xth piece in the row 
  where row = brd !! y     -- gets the yth row
  


{-| 
  Ex. 5: Return the valid moves that a cow in position (x,y) would have.

  [JUSTIFICATION]

  A cow can only move into empty spaces, one cell up, down, left and right of it, so ALL i had to do is check
  if these cells were empty and use filter to remove them if they werent. I did this by creating an isEmpty 
  function which takes a Coord and checks if it is empty ( it is also iPmortant to note that isEmpty will  
  return false if the the cell is not on the board and getPiece returns Nothing ). And i then used filter to 
  filter out the non empty or out of bounds cells. This seemed like the most straightforward way. I could have 
  instead created a function that took a list of coordinates and translated them into pieces in the same iteration 
  of the board, which would have been quicker as i wouldnt have to iterate through the board multiple times to 
  get each piece. However, this would have required a lot more and complicated code, the straightforward method 
  makes use of previously written functions so is more concise and understandable
-}


--Checks if a cell on a board is empty
isEmpty :: Board -> Coord -> Bool
isEmpty brd a = Just Empty == getPiece brd a 

validCowMoves :: Board -> Coord -> [Coord]
validCowMoves brd (x,y) = filter ( isEmpty brd)  [ (x+1,y), (x-1,y), (x,y+1), (x,y-1) ]


{-| 
  Ex. 6: Return the valid moves for a bean on the given team in position c.

  [JUSTIFICATION]
  The first thing to notice is that the function must check which player it is, to do this i created a and Eq 
  instance for Player and defined how the (==) operator would work with them. Instead i could have used pattern
  matching but this would have resulted in a lot of repeated code

  Next we need to understand where a bean move can move. A bean can move one cell up, down, to the left or to right 
  of it, provided if the cell is empty or contains an enemy bean. So i created a function that checked if a piece is
  a bean, and a function to check if a piece belongs to a player. I created these functions seperately as i imagined
  they would be useful for other functions ( i also created the an equivalent isCow method which isnt used in this
  question but is used in other functions).

  After i created all these auxillary functions, all i had to do is get a list of adjacent cells, and filter out the 
  cells that contained cow pieces or pieces from the same team (or off board cells). This method seemed like the most
  straightforward one
 -}

instance Eq Player where
  (==) :: Player -> Player -> Bool
  BluePlayer == BluePlayer   = True
  RedPlayer == RedPlayer     = True
  _ == _                     = False

--Checks if a piece belongs to a player 
belongsTo :: Player -> Maybe Piece -> Bool
belongsTo player piece
  | player == RedPlayer      = piece == Just(Red Bean) || piece == Just(Red Cow)
  | player == BluePlayer     = piece == Just(Blue Bean) || piece == Just(Blue Cow)
  | otherwise                = False 

--Checks if a piece is a Bean                          
isBean :: Maybe Piece -> Bool 
isBean piece = piece == Just (Red Bean) || piece == Just (Blue Bean)

--Checks if a piece is a Cow                          
isCow :: Maybe Piece -> Bool 
isCow piece =   piece == Just (Red Cow) || piece == Just (Blue Cow)


validBeanMoves :: Board -> Player -> Coord -> [Coord]
validBeanMoves brd player (x,y) = filter f allMoves
  where allMoves = [ (x+1,y), (x-1,y), (x,y+1), (x,y-1) ]
        f a = isBean piece && not ( belongsTo player piece) || piece == Just Empty        
              -- if the piece is a bean and does not belong to the player or if its empty
      
          where piece = getPiece brd a 
{-| 
  Ex. 7: Set a given (valid) coordinate to have a given piece (or Empty).

  First i created a function replace, which replaces the element at index i in a list, with a new value. I then took the 
  row specified by y, replaced the element specified by x with the new piece, then replaced the row on the board with the 
  new row with the new piece.
-}

-- Replaces the element at index i with val in a list 
replace :: Int -> a -> [a] -> [a]
replace i val list  = take i list ++ [val] ++ drop (i+1) list 

setCoord :: Board -> Coord -> Piece -> Board
setCoord brd (x,y) piece = replace y newRow brd  
  where 
    newRow = replace x piece row
    row = brd !! y


{-| 
  Ex. 8: Given two positions, return Just the updated board after moving the
  piece from the first position to the second. If the move was not valid, or 
  there was no piece in the first position, return Nothing instead.

  [JUSTIFICATION]

  Different types of pieces has a different set of valid moves, in fact, the different moves can be grouped 
  into three categories blue beans, red beans and cows.

  After determining which valid move corresponds to the piece if any, we have to check if the new position
  is part of the valid moves. If it is we then have to set the new position to have this piece using our 
  setCoord function, and the use that same function to cleae the previous position so that its empty.

-}


makeMove :: Board -> Coord -> Coord -> Maybe Board
makeMove brd pos1 pos2
  | pos2 `elem` validMoves           = Just newNewBoard    -- if pos2 is a valid move 
  | otherwise                        = Nothing             -- if pos1 is empty or pos2 is an invalid move 
  where
    newBoard = setCoord brd pos2 (fromJust piece) -- Puts the piece in the correct position 
    newNewBoard = setCoord newBoard pos1 Empty    -- Makes the initial position of the piece empty


    -- Sets validMoves according to the piece
    validMoves
      | piece == Just (Red Bean)     = validBeanMoves brd RedPlayer pos1     -- if the piece is a red bean
      | piece == Just (Blue Bean)    = validBeanMoves brd BluePlayer pos1    -- if the piece is a blue bean
      | isCow piece                  = validCowMoves brd pos1                -- if the piece is a cow 
      | otherwise                    = []                                    -- if the pos1 empty or out of bounds

    piece = getPiece brd pos1


{-| 
  Ex. 9: The game is drawn if the same setup is repeated, or after the fiftieth 
  move. Given a sequence of boards, most recent first, determine whether the 
  game is drawn in any position.

  [JUSTIFY]
-}
  
--Checks if an element occurs multiple times in a list 
isDuplicate :: (Eq a) => a -> [a] -> Bool
isDuplicate x list = 1 < countIF (==x) list 

gameIsDrawn :: [Board] -> Bool
gameIsDrawn brds = not (null duplicates) || length brds > 50 
  where duplicates = filter ( `isDuplicate` brds) brds 


{-| 
  Ex. 10: The game is won by Red if, on Blue's turn, one of their cows is 
  unable to move - and vice versa. Given a board, and the player whose move it 
  is, determine whether the game is won by the opponent.

  [JUSTIFY]
-}

-- Gets all the cows belonging to a player 
getCows :: Board -> Player -> [Coord]
getCows brd player =  [(colIndex, rowIndex) |
  (rowIndex, row) <- zip [0 .. ] brd,
  (colIndex, piece) <- zip [0 .. ] row,
  isCow (Just piece) && belongsTo player (Just piece) 
  ]


-- Gets all the beans belonging to a player 
getBeans :: Board -> Player -> [Coord]
getBeans brd player =  [(colIndex, rowIndex) |
  (rowIndex, row) <- zip [0 .. ] brd,
  (colIndex, piece) <- zip [0 .. ] row,
  isBean (Just piece) && belongsTo player (Just piece)
  ]

gameIsWon :: Board -> Player -> Bool
gameIsWon brd player = length moveableCows /= 2 
  where 
    cows = getCows brd player
    moveableCows = filter f cows
    f x = not $ null $ validCowMoves brd x


{-| 
  Finale: Implement a computer player to play the game of Bean's Gambit.
  Given the history of the game as a (nonempty) list, the AI should return an 
  appropriate next move as a pair (from, to).

  [JUSTIFY]
-}


-- Counts the number of enemy beans around a position on a board
countEnemyBeans :: Board -> Player -> Coord -> Int
countEnemyBeans brd player (x,y) = countIF f [ (x+1,y), (x-1,y), (x,y+1), (x,y-1) ]

  where f pos =  not (belongsTo player piece) && isBean piece  where piece =  getPiece brd pos

-- Counts the number of enemy cows around a position on a board
countEnemyCows :: Board -> Player -> Coord -> Int
countEnemyCows brd player (x,y) = countIF f [ (x+1,y), (x-1,y), (x,y+1), (x,y-1) ]

  where f pos =  not (belongsTo player piece) && isCow piece     where piece =  getPiece brd pos


-- Returns the a score for position on a board using my cow heuristic
getCowHeuristicScore :: Board  -> Coord -> Coord -> Int
getCowHeuristicScore brd from to   
  | toScore > fromScore       = 1 + 3-fromScore 
  | otherwise                 = 0
  where 
    fromScore = length $ validCowMoves brd from 
    toScore = 1 + length (validCowMoves brd to) 


-- Returns the a score for position on a board using my bean heuristic
getBeanHeuristicScore :: Board -> Player -> Coord -> Int
getBeanHeuristicScore brd player pos 
  | noSurroundingEnemies && attackingEnemy  && enemyCows == 2     =  5
  | noSurroundingEnemies && attackingEnemy  && enemyCows == 1     =  4
  | noSurroundingEnemies && attackingEnemy                        =  2
  | otherwise                                                     = -1

  where
    noSurroundingEnemies = countEnemyBeans brd player pos == 0
    attackingEnemy = not  (belongsTo player piece) && piece /= Just Empty
    enemyCows = countEnemyCows brd player pos 
    piece =  getPiece brd pos 


-- Gets the best move for a bean, returns (score,from,to)
getBestBeanMove :: Board -> Player -> Coord -> (Int,Coord,Coord)
getBestBeanMove brd player from = foldr f (-100,(-1,-1),(-1,-1)) (validBeanMoves brd player from)
  where 
    f newTo (score,_,to)  = 
      if newScore > score then (newScore,from,newTo) else (score,from,to)
      where newScore = getBeanHeuristicScore brd player newTo  


-- Gets the best move for a cow , returns (score,from,to)
getBestCowMove :: Board -> Coord -> (Int,Coord,Coord)
getBestCowMove brd  from = foldr f (-100,(-1,-1),(-1,-1)) (validCowMoves brd from)
  where 
    f newTo (score,_,to)  = 
      if newScore > score then (newScore,from,newTo) else (score,from,to)
      where newScore = getCowHeuristicScore brd from to 

-- Gets the best move out of a list of moves with their heuristic score (score,from,to)
getBestMove :: ([(Int, Coord, Coord)] ->(Int, Coord, Coord))
getBestMove  = foldr f (-100,(-1,-1),(-1,-1))  
  where 
    f (newScore,newFrom,newTo) (score,from,to) = 
          if newScore > score then (newScore,newFrom,newTo) else (score,from,to)


nextMove :: [Board] -> Player -> (Coord, Coord)
nextMove brds player = (from, to)
  where 
    brd = head brds 
    beans = getBeans brd player
    cows = getCows brd player
    bestBeanMoves =  map (getBestBeanMove brd player) beans
    bestCowMoves = map (getBestCowMove brd ) cows
    bestMoves = bestBeanMoves ++ bestCowMoves
    (_,from,to) =  getBestMove bestMoves  


