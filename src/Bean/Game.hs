module Bean.Game where

--------------------------------------------------------------------------------
-- This file should contain your complete work for the first coursework of 
-- CS141 Functional Programming.
-- 
-- USER ID: 2203275 
--
-- Before starting on this file, ensure that you have read the specification IN
-- ITS ENTIRETY and that you understand everything that is required from a good
-- solution.
--------------------------------------------------------------------------------

import Bean.Types( PieceType(..), Player(..), Piece(..), Coord, Board )
import Data.List (intercalate)
import Data.Maybe ( fromJust, isNothing)

{-| 
  Ex. 1: Write an expression representing the starting position for the game.


    [JUSTIFICATION]

    In this case i chose to hard code the board rather than use list comprehension 
    as the list is relatively small and using list comprehension would have
    resulted in more complicated code with a higher performance cost
  -}
startingPos :: Board
startingPos = [
  [Empty,Red Cow, Red Cow, Empty],
  [Red Bean, Red Bean, Red Bean, Red Bean],
  [Blue Bean, Blue Bean, Blue Bean, Blue Bean],
  [Empty,Blue Cow, Blue Cow, Empty]
  ]


{-|
  Ex. 2: Compute the balance of material, defined as the difference between the 
  number of Red beans and the number of Blue beans. Make use of explicit 
  recursion and pattern matching in your definition.

  
  [JUSTIFICATION]

 

  I created a general countIF function that counts the number elements in a list that meets 
  a condition, making use of foldr. 

  The balance function basically works by calculating the balance on each row and adding
  all of those balances together

  I could have instead created a more specific function that counted the number of red and blue pieces in the same 
  iteration, so it wouldnt have to iterate a row twice to count the red and blue beans. However, the countIF function 
  is useful proves very useful in other functions, and cuts down on repeted code massively. 
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

  This was just a case of using map on each row to map each piece into their string representation and 
  then concatinating the row, all by using concatMap, and then joining each row with \n using the intercalate 
  function. This method is straightforward and understandable, also, making use of intercalate reduces keeps 
  the code concise.
  

  Eq Instances

  I defined how the (==) operator would work according to the specification. 
  However i just defined as much as i needed to and used _ == _  to define the rest,
  this cuts down code, and also enures that no mistakes are made in the definitions,
  as an mistake is more likely if I defined every possible combination. Its also important 
  to note that the Eq instance for PieceType is required for the (==) to operate properly in
  my Eq instance of Piece

  Also for the Eq instance for piece, i could have used to show instances to compare pieces, however,
  technically speaking, the show instances for the pieces could be anything, so for maintainablity 
  purposes, comparing strict values is better

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
  
  This was just a case of getting the correct row using using the !! operator and then getting the 
  correct element in the row again using the !! operator, with the column and row of the piece being 
  defined by the x and y values of the Coord arguement respectively. This approach runs in O(mn) time
  (where m and n is the dimensions of the time), which is the most efficient it can be as we are a 
  searching a 2-dimensional list 

-}

getPiece :: Board -> Coord -> Maybe Piece
getPiece brd (x,y) = 
  if x<0 || x>3 || y<0 || y>3 then Nothing   --if the courdinates are out of bound 
  else Just (row !! x)                       -- return the xth piece in the row 
  where row = brd !! y                       -- gets the yth row
  


{-| 
  Ex. 5: Return the valid moves that a cow in position (x,y) would have.

  [JUSTIFICATION]

  A cow can only move into empty spaces, one cell up, down, left and right of it, so all i had to do is check
  if these cells were empty and use filter to remove them if they werent. I did this by creating an isEmpty 
  function which takes a Coord and checks if it is empty ( it is also important to note that isEmpty will  
  return false if the cell is not on the board and getPiece returns Nothing ). And i then used filter to 
  filter out the non empty or out of bounds cells. This seemed like the most straightforward, concise approach. 
  The isEmpty function is also very reusable.
-}


--Checks if a cell on a board is empty
isEmpty :: Board -> Coord -> Bool
isEmpty brd a = Just Empty == getPiece brd a 

validCowMoves :: Board -> Coord -> [Coord]
validCowMoves brd (x,y) = filter ( isEmpty brd)  [ (x+1,y), (x-1,y), (x,y+1), (x,y-1) ]


{-| 
  Ex. 6: Return the valid moves for a bean on the given team in position c.

  [JUSTIFICATION]
  The first thing to notice is that we need a function to check if a piece is a bean that belongs to a 
  specific player. So i created an Eq instance for Player and defined how the (==) operator would work 
  with them. Instead i could have used pattern matching but this would have resulted in a lot more code and
  a higher performance cost

  Next we need to understand where a bean move can move. A bean can move one cell up, down, to the left or to right 
  of it, provided if the cell is empty or contains an enemy bean. So i created a function that checked if a piece is
  a bean, and a function to check if a piece belongs to a player. I created these functions seperately as i imagined
  they would be useful for other functions ( i also created the an equivalent isCow method which isnt used in this
  question but is used in other functions).

  After i created all these auxillary functions, all i had to do is get a list of adjacent cells, and filter out the 
  cells that contained cow pieces or pieces from the same team (or off board cells) using the filter function. This 
  method seemed like the most straightforward and reusable due to all the auxillary functions. 
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

  First i created a function replace, which replaces the element at index i in a list, with a new value. I did
  this by using splitAt to split the list at index i. I then merged the left side and the tail of the right split.
  I could have instead used drop and take to get the left and right of the list around i so there would be no need
  for using tail, however tail is an O(1) operations and drop and take require their own iteration of the list wheras 
  splitAt only interates through the list once so using splitAt seems like the more  efficient method.

  In setCoord, I took the row specified by y, replaced the element specified by x with the new piece, then 
  replaced the row on the board with the new row with the new piece.
-}

-- Replaces the element at index i with val in a list 
replace :: Int -> a -> [a] -> [a]
replace i val list  = left ++ [val] ++ tail right
  where  (left,right) = splitAt i list
         

setCoord :: Board -> Coord -> Piece -> Board
setCoord brd (x,y) piece = replace y newRow brd  
  where 
    newRow = replace x piece row -- replaces the xth piece in the row with the new piece
    row = brd !! y -- gets yth row in the board


{-| 
  Ex. 8: Given two positions, return Just the updated board after moving the
  piece from the first position to the second. If the move was not valid, or 
  there was no piece in the first position, return Nothing instead.

  [JUSTIFICATION]

  Different types of pieces have a different set of valid moves, in fact, the different moves can be grouped 
  into four categories blue beans, red beans, cows and empty ( in which case it will have no valid moves).

  After determining which valid move corresponds to the piece if any, we have to check if the new position
  is an element of the valid moves. If it is we then have to set the new position to have this piece using our 
  setCoord function, and the use that same function to clear the previous position so that it holds an empty
  piece.

  This approach seems like the most obvious and easiest approach. It follows the questions specification exactly,
  amd the guards make it easy to see which condition causes which outcome

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

  [JUSTIFICATION]
  First i created a function, isDuplicate, that checked if an item is a duplicate by counting  the
  number of occurrences  of the element in the list using the countIF function, and checking then if its more than 1. 
  This seemed like the most concise and sensible implementation as it uses previously implemented functions


  I then filtered out the boards in the list of boards that were not duplicates and checked if the list was not
  empty and also checked if the length of the list of boards is greater than 50. 

  Also notice that isDuplicate has an Eq constraint on the list arguement, however we know that it will work when 
  we pass in a list of boards because the rows in the board have an Eq instance because the rows elements (Pieces)
  have an Eq instance 
-}
  
--Checks if an element occurs multiple times in a list 
isDuplicate :: (Eq a) => a -> [a] -> Bool
isDuplicate x list = 1 < countIF (==x) list 

gameIsDrawn :: [Board] -> Bool
gameIsDrawn brds = countIF ( `isDuplicate` brds) brds /= 0  || length brds > 50 


{-| 
  Ex. 10: The game is won by Red if, on Blue's turn, one of their cows is 
  unable to move - and vice versa. Given a board, and the player whose move it 
  is, determine whether the game is won by the opponent.

  [JUSTIFICATION]
  A game is won if the player's cows have nowhere to move i.e have no valid movesSo I first created a function, 
  getCows, which gets the  position of all cows belonging to a player ( and an equivalent getBeans function that 
  isnt used in this question but is in future functions). Within list comprehension, i used the zip function on the 
  board to enumerate each row and then used the zip function again to enumerate the pieces the in each row. This 
  meant i had access to both the piece and its position within the generator. I was then able to use the isCow 
  function to check if the piece was a cow and also the belongsTo function to check if the piece belonged to the 
  player, so the position of each piece would only be in the generated list if it passed these two conditions.
  This method runs in O(nm) time (where n and m are the dimensions of the board). Alternatively, i could have
  iterated through every position on the board and use getPiece to get the piece in that position - this would be a 
  less efficient method which runs in O((nm)^2). Any alternative, more specific methods would have made the function more 
  complex and  less reusable 


  The gameIsWon function was just a simple case of using getCows to get all the players cows, and filtering
  out any of the cows with no valid moves, then returning whether the filtered list is empty 

-}

-- Gets all the cows belonging to a player 
getCows :: Board -> Player -> [Coord]
getCows brd player =  [(colIndex, rowIndex) |
  (rowIndex, row) <- zip [0 .. ] brd, -- enumerating row
  (colIndex, piece) <- zip [0 .. ] row, -- enumerating piece 
  isCow (Just piece) && belongsTo player (Just piece) 
  ]


-- Gets all the beans belonging to a player 
getBeans :: Board -> Player -> [Coord]
getBeans brd player =  [(colIndex, rowIndex) |
  (rowIndex, row) <- zip [0 .. ] brd,-- enumerating row
  (colIndex, piece) <- zip [0 .. ] row,-- enumerating pieces
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

  [JUSTIFICATION]

  COW HEURISTIC Explanation 

  There are some obvious heuristics to account for, which are the moves that would result in the end of 
  the game. 
    1. If a move results in a win the move gets a score of 10000
    2. If a move results in a draw it gets a score of -5000
    3. If a move results in a loss it gets a score of -10000

  These scores are  not be reachable using the other heuristics. This ensures that the AI will always win the game 
  in the next move  if possible and will only lose or draw the game if no other move is is available. A draw move is
  given more points than a loss so the AI will choos a draw over a loss. This also means that the AI  will keep 
  making moves until either a win is possible in the next move, or no other move is possible

  If the move will not result in the end of a game, the score of the move will be calculated using:

    1.The difference between the number of available moves between the current position and the new position 
      
    - so the larger the (positive) difference the more points is awarded to the move 

    - this is to ensure the AI's cows always has space to move as the less free space a cow has, the closer the cow
      is to getting trapped .

    - also cows moving from a place with less available moves are awarded more points so that they are more likely to 
      be moved to a place with more available moves. This ensures that both of the AI's cows have a good amount of 
      available space


    2.The number of cows on the same team that are adjacent to the new position - this is because if a cow is 
      adjacent to another cow on the same teame, the number of available  moves for both cows is reduced

      - so the larger the number of the player's cows that are adjacent to the new psotion, the less points awarded 
        to the move 


    I adjusted the weighting of these two heuristics so maximise the AI's win rate. Also note that in order 
    to maximaise the AI's win rate, it was important to consider each heuristic's weighting relative to
    each other ( including the beans heuristic weighting )
    

  BEAN HEURISTIC Explanation 

  The score given to a bean move follows fairly different heuristics. The points for whether a move results
  in a win, draw or a loss will remain to same as we want the AI to make the same decision if the next 
  move results in these circumstances 

  Otherwise the heuristic score for a bean move is based on:

    1.The number of enemy beans that are adjacent to the new position 
    
      - this is because the more opposing beans there are, the more likely the AI's bean will be taken, this 
        will result in less of the player's beans on the board making their cows more vulnerable
      
      -so the more adjacent enemy beans at the new position, the lower the score awarded to the move


    2.The number of cows on the same team that are adjacent to the new position 
    
      - this is because if a cow is adjacent to another cow on the same team, the number of available  moves for ypur 
        cows are reduced

      - so the larger the number of the player's cows that are adjacent to the new psotion, the less points awarded 
        to the move 


    3.Whether the player is taking an enemy bean or not
      
      - This is because taking an enemy bean makes it harder for the other player to trap the AI's cow. Also taking a 
        bean reduces the chance of a draw as it means previous boards cant be be recreated - this is good as it gives 
        theAI more opportunities to win 

    4.The number of enemy cows surrounding the new position  

      - This is because , to win the game, you must trap one of the opposing player's cows, so if a bean is adjacent
        to an opposing cow, the adjacent cow is closer to being trapped

      - So the more opposing cows that are adjacent to the new position, the more points are awarded to the move


  Implementing the Heuristics

  I first created specific functions that checked whether the move a would result in a win or a draw. To do this 
  i passed the move into the makeMove function, which returns Maybe Board which containsthe new board if the move
  was valid or Nothing if the move was invalid. if Nothing is returned from makeMove i.e the move is invalid, all the
  functions will return False 

    1.isDrawMove
      To check if the move resulted in a draw i used the fromJust function to unwrap the brd then 
      added the new brd to the head list of boards then passed the new list of boards into the gameIsDrawn function to
      check if the move results in a Draw

      - this seems like the most straightforward, concise approach as it makes use of previously implemented functions

    2.isWinMove
      This function similarly uses the makeMove and then fromJust to get the new board with the updated move, but 
      instead i pass in the new board along with the opposing player into gameIsWon to check if the current player 
      would win
      
      I also created a not' function which returns the opposing player to the player passed in

    3. To check if a move is a loss i call isWinMove and pass in the opposing player using the not' function 
  
  
  To calculate the heuristcs i did the following:

    1. Adjacent enemy cows:
      
      I created a function called countAdjCows which counts the number of cows which are adjacent to a position, using 
      the countIF function to iterate and count through the list and the isCow and the belongsTo functions  to get the 
      cows belonging to the opposing player

      This implementation of the countAdjCows functions seemed the most sensible considering its use of previously
      written functions.


    2. Adjacent friendly cows:

      This is calculated the same way as getting the adjacent opposing cows but passing in the AI's player instead 
      of the opposing player


    3. Adjacent enemy beans
      
      Implemented using the equivalent bean version to counting the adjacent cows

    4.A bean is attacking another bean if the new position has a bean from the opposing bean. I can easily check 
      these conditions are met using the isBean and belongsTo function. These are both O(1) operations so we it
      is already a very efficient method

  Comparing Moves 

  The getCowHeuristicScore and  getBeanHeuristicScore calculates and returns a single move with their heuristic 
  score using these methods to calculate each heuristic and using simple guard conditions to see which heuristic is 
  applies. I made seperate functions for the heuristic for each piece as it makes the code more readable maintainable 
  - so it would be easier to adjust the weighting of each heuristic as i can more easily see how the cows and beans 
  heuristic compare. Also using guard conditions seemed like the standard, straightforward approach, also making easy
  to see which condition corresponds with each heuristic score

  Now that we are able to calculate the heuristic of each move, we need a way of comparing the heuristic. This is one
  of the reasons i created type HMove, which stands for Heuristic Move, it holds the score and the move as  
  ( score, from position, to position ). This type makes the code more readable and maintainable as i didnt have to 
  repeat using (Int,Coord,Coord), and it also meant i only had to change the HMove type if i wanted to change how i 
  was going to compare moves. 

  The getBestBeanMove and getBestCowMove gets the best move for a single piece. The functions use validCowMoves and 
  validBeanMoves to get the possible moves of a piece, and use foldr to: iterate through the list, use to the 
  getCowHeuristicScore and getBeanHeuristicScore to calculate the heuristic score, compare each move and returns best 
  move for that piece. Seperating getBestBeanMove and getBestCowMove means that we dont have one large, hard to read 
  function with multiple conditions. Using foldr seemed like appropriate as i can calculate the best score and
  move in one iteration of the list of valid moves ( list of new position coordinates ) 

  getBestMove works similarly, however it gets the best move out of a list of HMoves also also using foldr and comparing
  each move without calculating anything 


Putting it all together: nextMove

One of the biggest reasons I had multiple seperate more specific functions ( seperated functions for beans and cows ) rather
than less more complex functions was so to make it make the implementation of the AI more readable and easier to understand - 
it makes it clear what steps are happening in sequence to make the AI function. This ultimately results in a very clear and 
sequential, straightforward implementation of nextMove:

    1. get the positions of all player's beans - using getBeans
    2. get the psotions of all player's cows   - using getCows
    3. get the best move for all the player's beans - by mapping the bean positions using getBestBeanMove
    4. get the best moves for all the player's cow  - by mapping the cow positions using getCowMove
    5. get the best move out all the best cow and bean moves 

Considering all the auxillary functions, this seems like the best implementation.

  
-}

        
isWinMove :: Board -> Player -> Coord -> Coord -> Bool
isWinMove brd player from to 
  | isNothing newBrd        = False -- if the move is invalid
  | otherwise               = gameIsWon (fromJust newBrd) (not' player)
  
  where newBrd = makeMove brd from to  -- The board after the move

       
isDrawMove :: [Board] -> Coord -> Coord -> Bool
isDrawMove brds from to 
  | isNothing newBrd       = False -- if the move is invalid
  | otherwise              = gameIsDrawn $ fromJust newBrd : brds 
                                                    -- Adding new board as the head to the list of previous boards 
  where 
    brd = head brds 
    newBrd = makeMove brd from to  -- The board after the move


-- Counts the number of enemy beans around a position on a board
countAdjBeans :: Board -> Player -> Coord -> Int
countAdjBeans brd player (x,y) = countIF f [ (x+1,y), (x-1,y), (x,y+1), (x,y-1) ]

  where f pos =  belongsTo player piece && isBean piece     where piece =  getPiece brd pos

-- Counts the number of enemy cows around a position on a board
countAdjCows :: Board -> Player -> Coord -> Int
countAdjCows brd player (x,y) = countIF f [ (x+1,y), (x-1,y), (x,y+1), (x,y-1) ]

  where f pos =  belongsTo player piece && isCow piece     where piece =  getPiece brd pos


--Returns the opposing player
not' :: Player -> Player
not' RedPlayer = BluePlayer
not' BluePlayer = RedPlayer


-- Returns the a score for position on a board using my cow heuristic
getCowHeuristicScore :: [Board] -> Player -> Coord -> Coord -> Int
getCowHeuristicScore brds player from to 
  | isWinMove brd player from to               =  10000     -- if the move would result in a win
  | isDrawMove brds from to                    = -5000      -- if the move would result in a draw
  | isWinMove brd (not' player) from to        = -10000     -- if the move would result in a loss
  | otherwise                                  = (to_availableMoves - 2*from_availableMoves)*4 - 7*friendlyCows 
  where 
    from_availableMoves = length $ validCowMoves brd from  -- amount of available moves in current position 
    to_availableMoves = 1 + length (validCowMoves brd to)  -- amount of available moves in new position
    friendlyCows = countAdjCows brd player to - 1          -- number of surrounding cows belonging to the player
    brd = head brds                                        {-
                                                              taking away 1 because the current position contains a cow that will be
                                                              counted but will not be there when the move is actually carried out 
                                                          -}
     

-- Returns the a score for position on a board using my bean heuristic
getBeanHeuristicScore :: [Board]  -> Player -> Coord -> Coord -> Int
getBeanHeuristicScore brds player from to 
  | isWinMove brd player from to               =  10000     -- if the move would result in a win
  | isDrawMove brds from to                    = -10        -- if the move would result in a draw
  | isWinMove brd (not' player) from to        = -10000     -- if the move would result in a loss
  | otherwise                                  =  15*attackingEnemy + 4*enemyCows - 7*friendlyCows - 5*enemyBeans + 8

  where
    enemyBeans = countAdjBeans brd ( not' player ) to  -- number of surrounding beans belonging to the other player
    friendlyCows = countAdjCows brd player to          -- number of surrounding cows belonging to the player
    enemyCows = countAdjCows brd ( not' player ) to    -- number of surrounding cows belonging to the other player
    attackingEnemy = if not  (belongsTo player piece) && isBean piece then 1 else 0 --whether the bean is taking another bean 
                     --if the piece belongs to the opposing team and is a bean
    piece =  getPiece brd to 
    brd = head brds 


-- this type will hold a move along with their heuristic score 
type HMove = (Int,Coord,Coord)

dummyMove :: HMove 
dummyMove = (-1000000*1000000, (-1,-1), (-1,-1) )

-- Gets the best move for a bean, returns (score,from,to)
getBestBeanMove :: [Board] -> Player -> Coord -> HMove
getBestBeanMove brds player from = foldr f dummyMove validMoves
  where 
    brd = head brds 
    validMoves = validBeanMoves brd player from

    f newTo (score,_,to) = if newScore > score then (newScore,from,newTo) else (score,from,to)
      where newScore = getBeanHeuristicScore brds player from newTo  


-- Gets the best move for a cow , returns (score,from,to)
getBestCowMove :: [Board] -> Player -> Coord -> HMove
getBestCowMove brds player from = foldr f dummyMove validMoves
  where 
    brd = head brds
    validMoves = validCowMoves brd from

    f newTo (score,_,to)  = if newScore > score then (newScore,from,newTo) else (score,from,to)    
      where newScore = getCowHeuristicScore brds player from to 


-- Gets the best move out of a list of moves with their heuristic score (score,from,to)
getBestMove :: [HMove] -> HMove
getBestMove  = foldr f dummyMove  
  where 
    f (newScore,newFrom,newTo) (score,from,to) = if newScore > score then (newScore,newFrom,newTo) else (score,from,to)


nextMove :: [Board] -> Player -> (Coord, Coord)
nextMove brds player = (from, to)
  where 
    brd = head brds 
    beans = getBeans brd player                                  -- all the player's beans
    cows = getCows brd player                                    -- all the player's cows 
    bestBeanMoves =  map (getBestBeanMove brds player) beans     -- a list of all the possible bean moves and their heuristic score 
    bestCowMoves = map (getBestCowMove brds player) cows         -- a list of all the possible cows moves and their heuristic score 
    bestMoves = bestCowMoves ++ bestBeanMoves                    -- a list of the best moves from all the player's pieces and their score 
    (_,from,to) =  getBestMove bestMoves                         -- the best move


