import Data.Char (ord)

type Location = (Char, Int)
data Player = White | Black deriving (Show, Eq)
data Piece = P Location | N Location | K Location | Q Location | B Location | R Location deriving (Show, Eq)
type Board = (Player, [Piece], [Piece])

setBoard :: Board
setBoard=(White, [R ('h',1),N ('g',1),B ('f',1),K ('e',1),Q ('d',1),B ('c',1),N ('b',1),R ('a',1),P ('h',2),P ('g',2),P ('f',2),P ('e',2),P ('d',2),P ('c',2),P ('b',2),P ('a',2)] ,[R ('h',8),N ('g',8),B ('f',8),K ('e',8),Q ('d',8),B ('c',8),N ('b',8),R ('a',8),P ('h',7),P ('g',7),P ('f',7),P ('e',7),P ('d',7),P ('c',7),P ('b',7),P ('a',7)])

isLegalKingMove :: (Char, Int) -> (Char, Int) -> Bool
isLegalKingMove (x1, y1) (x2, y2) =
  abs (colDiff x1 x2) <= 1 && abs (y1 - y2) <= 1

isLegalBishopMove :: (Char, Int) -> (Char, Int) -> Bool
isLegalBishopMove (x1, y1) (x2, y2) =
  abs (colDiff x1 x2) == abs (y1 - y2)

isLegalQueenMove :: (Char, Int) -> (Char, Int) -> Bool
isLegalQueenMove from to = isLegalBishopMove from to || isLegalRookMove from to

isLegalRookMove :: (Char, Int) -> (Char, Int) -> Bool
isLegalRookMove (x1, y1) (x2, y2) =
  x1 == x2 || y1 == y2

isLegalKnightMove :: (Char, Int) -> (Char, Int) -> Bool
isLegalKnightMove (x1, y1) (x2, y2) =
  abs (colDiff x1 x2) * abs (y1 - y2) == 2  

colDiff :: Char -> Char -> Int
colDiff c1 c2 = abs (ord c1 - ord c2)

getLocation :: Piece -> Location
getLocation (P loc) = loc
getLocation (N loc) = loc
getLocation (K loc) = loc
getLocation (Q loc) = loc
getLocation (R loc) = loc
getLocation (B loc) = loc
--getLocation _ = error "Invalid piece" 

isCellEmpty :: Board -> Location -> Bool
isCellEmpty (_, white, black) location = not (elem location (map getLocation (white ++ black)))
  
    
	
emptyCellsIncol :: Board -> Location -> Location -> Location -> Bool
emptyCellsIncol b (x,y) (x1, y1) (x2, y2)
  | x1 == x2 && y1 == y2 = True
  | (x1 /= x || y1 /= y) && not (isCellEmpty b (x1, y1)) = False
  | x1 /= x2 =False
  | y2 > y1 = emptyCellsIncol b (x,y) (x1, y1 + 1) (x2, y2)
  | y1 > y2 = emptyCellsIncol b (x,y) (x1, y1 - 1) (x2, y2)
  | otherwise = False
  
emptyCellsInrow :: Board -> Location -> Location -> Location -> Bool
emptyCellsInrow b (x,y) (x1, y1) (x2, y2)
  | x1 == x2 && y1 == y2 = True
  | (x1 /= x || y1 /= y) && not (isCellEmpty b (x1, y1)) = False
  | y1 /= y2 =False
  | x2 > x1 = emptyCellsInrow b (x,y) (toEnum((ord x1) +1), y1) (x2, y2)
  | x1 > x2 = emptyCellsInrow b (x,y) (toEnum((ord x1) -1), y1) (x2, y2)
  | otherwise = False
  
emptyCellsDiagonal :: Board -> Location -> Location -> Location -> Bool
emptyCellsDiagonal b (x,y) (x1, y1) (x2, y2)
  |x1 == x2 && y1 == y2 = True
  |(x1 /= x || y1 /= y) && not (isCellEmpty b (x1, y1)) = False
--  |abs (x2 - x1) /= abs (y2 - y1) = False
  | y2 > y1 && x2 > x1 = emptyCellsDiagonal b (x,y) (toEnum((ord x1) +1), y1 + 1) (x2, y2)
  | y2 > y1 && x2 < x1 = emptyCellsDiagonal b (x,y) (toEnum((ord x1) -1), y1 + 1) (x2, y2)
  | y2 < y1 && x2 > x1 = emptyCellsDiagonal b (x,y) (toEnum((ord x1) +1), y1 - 1) (x2, y2)
  | y2 < y1 && x2 < x1 = emptyCellsDiagonal b (x,y) (toEnum((ord x1) -1), y1 - 1) (x2, y2)
  | otherwise = False 

canKill :: Piece -> Board-> Location -> Bool
canKill p (player,whiteP,blackP) loc
  |isCellEmpty (player,whiteP,blackP) loc=True
  |elem p whiteP  && elem loc (map getLocation (blackP))=True
  |elem p blackP  && elem loc (map getLocation (whiteP))=True 
  |otherwise=False 
  
--isLegalPawnMove :: Board -> (Char, Int) -> (Char, Int) -> Bool
isLegalWhitePawnMove (White,_,_) (x1, y1) (x2, y2)
  | y2 == y1 + 1 && x2 == x1 = True -- move one square forward
  | y2 == y1 + 2 && x2 == x1 && y1 == 2 = True -- move two squares forward from starting position
  | not (isCellEmpty (x2,y2)) && y2 == y1 + 1 && abs (colDiff x1 x2) == 1 = True -- capture an opponent's piece diagonally
  | otherwise = False

isLegalBlackPawnMove (Black,_,_) (x1, y1) (x2, y2)
  | y2 == y1 - 1 && x2 == x1 = True -- move one square forward
  | y2 == y1 - 2 && x2 == x1 && y1 == 7 = True -- move two squares forward from starting position
  | not (isCellEmpty (x2,y2)) && y2 == y1 - 1 && abs (colDiff x1 x2) == 1 = True -- capture an opponent's piece diagonally
  | otherwise = False


--isLegal :: Piece -> Board -> Location -> Bool
isLegal (N loc) board (x2,y2)= if(isLegalKnightMove loc (x2,y2) && canKill (N loc) board (x2,y2)) then True else False
isLegal (K loc) board (x2,y2)= if(isLegalKingMove loc (x2,y2) && canKill (K loc) board (x2,y2)) then True else False
isLegal (Q loc) board (x2,y2)= if(isLegalQueenMove loc (x2,y2) && (emptyCellsIncol board loc loc (x2,y2) || emptyCellsInrow board loc loc (x2,y2) || emptyCellsDiagonal board loc loc (x2,y2)) && canKill (Q loc) board (x2,y2)) then True else False  
isLegal (R loc) board (x2,y2)= if(isLegalRookMove loc (x2,y2) && (emptyCellsIncol board loc loc (x2,y2) || emptyCellsInrow board loc loc (x2,y2)) && canKill (R loc) board (x2,y2)) then True else False  
isLegal (B loc) board (x2,y2)= if(isLegalBishopMove loc (x2,y2) && emptyCellsDiagonal board loc loc (x2,y2) && canKill (B loc) board (x2,y2)) then True else False
isLegal (P loc) (player,white,black) (x2,y2)= if((((elem (P loc) white) && isLegalWhitePawnMove (player,white,black) loc (x2,y2)) || ((elem (P loc) black) && isLegalBlackPawnMove (player,white,black) loc (x2,y2)))   && canKill (P loc) (player,white,black) (x2,y2)) then True else False
    
suggestHelper :: Piece -> Board ->Location-> [Location]
suggestHelper p board (x,y)
	|x=='h' && y==8 = []
    |y==8 = suggestHelper p board (toEnum((ord x) +1),1)
    |isLegal p board (x,y)=(x,y):suggestHelper p board (x,y+1)	 
    |otherwise=suggestHelper p board (x,y+1)	   

suggestMove :: Piece -> Board -> [Location]
suggestMove p board = suggestHelper p board ('a',1)
    
removeElement :: Location->[Piece]->[Piece]
removeElement _ [] = []
removeElement x (y:ys)
    | x == getLocation y   = removeElement x ys
    | otherwise = y : removeElement x ys
	
changeLocation :: Piece->Location->Piece
changeLocation (P loc) newLoc= (P newLoc)
changeLocation (N loc) newLoc= (N newLoc)
changeLocation (K loc) newLoc= (K newLoc)
changeLocation (Q loc) newLoc= (Q newLoc)
changeLocation (R loc) newLoc= (R newLoc)
changeLocation (B loc) newLoc= (B newLoc)

replaceElement :: Eq a => a -> a -> [a] -> [a]
replaceElement _ _ [] = []
replaceElement old new (x:xs)
    | old == x  = new : replaceElement old new xs
    | otherwise = x : replaceElement old new xs

move:: Piece -> Location -> Board -> Board
move p loc (player,whiteP,blackP)
    |elem p whiteP && player==Black =error "This is Black player's turn, White can't move."
    |elem p blackP && player==White =error "This is White player's turn, Black can't move." 
	|not (isLegal p (player,whiteP,blackP) loc) = error ("Illegal move for piece "  ++ show p )
	|player==White && isCellEmpty (player,whiteP,blackP) loc = (Black,replaceElement p (changeLocation p loc) whiteP,blackP)
	|player==White && not (isCellEmpty (player,whiteP,blackP) loc) = (Black,replaceElement p (changeLocation p loc) whiteP,removeElement loc blackP)
	|player==Black && isCellEmpty (player,whiteP,blackP) loc = (White,whiteP,replaceElement p (changeLocation p loc) blackP)
	|player==Black && not (isCellEmpty (player,whiteP,blackP) loc) = (White,removeElement loc whiteP,replaceElement p (changeLocation p loc) blackP)	


