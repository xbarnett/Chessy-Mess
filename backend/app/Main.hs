module Main (main) where

import qualified Data.Map.Strict as M
import qualified Data.Text.Lazy as T
import qualified Network.WebSockets as N

data PieceType = Pawn | Rook | Knight | Bishop | Queen | King

data Alignment = LG | NG | CG | LN | TN | CN | LE | NE | CE

data Player = Red | Blue
  deriving Eq

data Piece = Piece {
  pieceType :: PieceType,
  pieceAlignment :: Alignment
}

data State = State {
  specifics :: M.Map (Integer, Integer) (Maybe Piece),
  generals :: (Integer, Integer) -> Maybe Piece,
  toMove :: Player
}

data Move = Move {
  moving :: Player,
  from :: (Integer, Integer),
  to :: (Integer, Integer)
}

nextPlayer :: Player -> Player
nextPlayer Red = Blue
nextPlayer Blue = Red

initialState :: State
initialState = State {
  specifics = M.fromList [
      ((0, -3), Just (Piece {pieceType = King, pieceAlignment = LE})),
      ((0, 3), Just (Piece {pieceType = King, pieceAlignment = LG}))
    ],
  generals = const Nothing,
  toMove = Red
}

pieceAt :: State -> (Integer, Integer) -> Maybe Piece
pieceAt s coord = case M.lookup coord (specifics s) of
  Just mp -> mp
  Nothing -> generals s coord

ofPlayer :: Maybe Piece -> Player -> Bool
ofPlayer Nothing _ = False
ofPlayer (Just piece) player = case (pieceAlignment piece, player) of
  (LG, Blue) -> True
  (NG, Blue) -> True
  (CG, Blue) -> True
  (LE, Red) -> True
  (NE, Red) -> True
  (CE, Red) -> True
  _ -> False

-- returns the piece being moved, if move is legal
isLegalMove :: State -> Move -> Maybe Piece
isLegalMove s m = case pieceAt s (from m) of
  Nothing -> Nothing
  Just piece -> if toMove s /= moving m then Nothing else
    case pieceType piece of
      King -> if isKingMove s m then Just piece else Nothing
      _ -> Nothing

isKingMove :: State -> Move -> Bool
isKingMove s m = elem (x2 - x1, y2 - y1)
  [(-1, -1), (-1, 0), (-1, 1), (0, -1), (0, 1), (1, -1), (1, 0), (1, 1)] &&
  not (ofPlayer (pieceAt s (x2, y2)) (moving m)) where
  (x1, y1) = from m
  (x2, y2) = to m

makeMove :: State -> Move -> Maybe State
makeMove s m = case isLegalMove s m of
  Nothing -> Nothing
  Just piece -> Just $
    State {
      specifics = M.insert (from m) Nothing (
          M.insert (to m) (Just piece) (specifics s)),
      generals = generals s,
      toMove = nextPlayer (toMove s)
    }

game :: State -> IO State
game s = do
  line <- getLine
  let (x1, y1, x2, y2) = read line
  let n = makeMove s (Move {from = (x1, y1), to = (x2, y2), moving = toMove s})
  case n of
    Nothing -> do
      putStrLn "invalid move"
      game s
    Just s2 -> game s2

main :: IO ()
main = do
  putStrLn "hi there!"
  N.runServer "localhost" 8000 $ \pc -> do
    c <- N.acceptRequest pc
    N.sendTextData c (T.pack "hello from mr server")
