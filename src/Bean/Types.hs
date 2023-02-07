module Bean.Types where

import Data.Char (toLower)

--------------------------------------------------------------------------------
-- Type Definitions


-- | The board is a two-dimensional list of pieces.
type Board = [[Piece]]

-- | A coordinate represents the (row,column) of the square in question. 
--   Zero-indexed from the top-left of the board.
type Coord = (Int,Int)

-- | A Piece is one square of the board; it may be empty or contain a Red
--   piece or a Blue piece.
data Piece = Empty | Red PieceType | Blue PieceType

-- | The corresponding player is either Red or Blue.
data Player = RedPlayer | BluePlayer

-- | There are two pieces: Beans and Cows.
data PieceType
  = Bean
  | Cow


--------------------------------------------------------------------------------
-- Provided instances

-- You may make use of these instances when implementing your own instances 
-- as appropriate.


instance Show PieceType where
  show :: PieceType -> String
  show Bean = "B"
  show Cow  = "C"

instance Show Piece where
  show :: Piece -> String
  show Empty     = "."
  show (Red pt)  = "\27[31m" ++              show pt ++ "\27[0m"
  show (Blue pt) = "\27[34m" ++ map toLower (show pt) ++ "\27[0m"

instance Show Player where
  show :: Player -> String
  show RedPlayer = "\27[31mRed\27[0m player"
  show BluePlayer = "\27[34mBlue\27[0m player"
