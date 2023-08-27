{-# LANGUAGE DeriveGeneric #-}

module Main (main) where

import Control.Concurrent.MVar
import Control.Exception
import qualified Data.Aeson as J
import qualified Data.ByteString.Lazy as BS
import qualified Data.Char as C
import qualified Data.Map.Strict as M
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.Encoding as T
import GHC.Generics
import qualified Network.WebSockets as N
import qualified Network.WebSockets.Connection as N
import qualified Data.ByteString.Lazy.UTF8 as BSU

data PieceType = Pawn | Rook | Knight | Bishop | Queen | King
  deriving (Generic, Show, Eq)

instance J.ToJSON PieceType where
  toEncoding = J.genericToEncoding J.defaultOptions

instance J.FromJSON PieceType where

data Alignment = LG | NG | CG | LN | TN | CN | LE | NE | CE
  deriving (Generic, Show, Eq)

alignmentToPlayer :: Alignment -> [Player]
alignmentToPlayer a = case a of
  LG -> [Blue]
  NG -> [Blue]
  CG -> [Blue]
  LE -> [Red]
  NE -> [Red]
  CE -> [Red]
  _ -> []

isRedKing :: Maybe Piece -> Bool
isRedKing Nothing = False
isRedKing (Just p) = pieceType p == King &&
  alignmentToPlayer (pieceAlignment p) == [Red]

isBlueKing :: Maybe Piece -> Bool
isBlueKing Nothing = False
isBlueKing (Just p) = pieceType p == King &&
  alignmentToPlayer (pieceAlignment p) == [Blue]

instance J.ToJSON Alignment where
  toEncoding = J.genericToEncoding J.defaultOptions

instance J.FromJSON Alignment where

data Player = Red | Blue
  deriving (Generic, Show, Eq)

instance J.ToJSON Player where
  toEncoding = J.genericToEncoding J.defaultOptions

instance J.FromJSON Player where

data Piece = Piece {
  pieceType :: PieceType,
  pieceAlignment :: Alignment
} deriving (Generic, Show, Eq)

instance J.ToJSON Piece where
  toEncoding = J.genericToEncoding J.defaultOptions

instance J.FromJSON Piece where

data State = State {
  specifics :: M.Map (Integer, Integer) (Maybe Piece), -- king is here
  generals :: (Integer, Integer) -> Maybe Piece,
  toMove :: Player
}

data Move = Move {
  mfrom :: (Integer, Integer),
  mto :: (Integer, Integer)
} deriving (Generic, Show)

instance J.ToJSON Move where
  toEncoding = J.genericToEncoding J.defaultOptions

instance J.FromJSON Move where

instance J.ToJSON Request where
  toEncoding = J.genericToEncoding J.defaultOptions

instance J.FromJSON Request

listAttacking :: State -> (Integer, Integer) -> (Integer, Integer) ->
  (Integer, Integer) -> Response
listAttacking s p (x1, y1) (x2, y2) = ResponseAttacking $ if
  ofPlayer (pieceAt s p) (toMove s) then
  [(x, y) | x <- [x1 .. x2], y <- [y1 .. y2], isLegalMove s
    (Move {mfrom = p, mto = (x, y)}) /= Nothing] else []

instance J.ToJSON Response where
  toEncoding = J.genericToEncoding J.defaultOptions

instance J.FromJSON Response

nextPlayer :: Player -> Player
nextPlayer Red = Blue
nextPlayer Blue = Red

initialState :: State
initialState = State {
  specifics = M.fromList [
      ((0, -3), Just (Piece {pieceType = King, pieceAlignment = LE})),
      ((1, -3), Just (Piece {pieceType = Queen, pieceAlignment = LE})),
      ((0, -2), Just (Piece {pieceType = Rook, pieceAlignment = LE})),
      ((0, -4), Just (Piece {pieceType = Bishop, pieceAlignment = LE})),
      ((-1, -3), Just (Piece {pieceType = Knight, pieceAlignment = LE})),
      ((0, 3), Just (Piece {pieceType = King, pieceAlignment = LG})),
      ((0, 4), Just (Piece {pieceType = Bishop, pieceAlignment = LG})),
      ((0, 2), Just (Piece {pieceType = Rook, pieceAlignment = LG})),
      ((1, 3), Just (Piece {pieceType = Queen, pieceAlignment = LG})),
      ((-1, 3), Just (Piece {pieceType = Knight, pieceAlignment = LG}))
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
isLegalMove s m = case winnerOfGame s of
  Nothing -> case pieceAt s (mfrom m) of
    Nothing -> Nothing
    Just piece -> if not (ofPlayer (Just piece) (toMove s)) then Nothing else
      case pieceType piece of
        King -> if isKingMove s m then Just piece else Nothing
        Knight -> if isKnightMove s m then Just piece else Nothing
        Rook -> if isRookMove s m then Just piece else Nothing
        Bishop -> if isBishopMove s m then Just piece else Nothing
        Queen -> if isQueenMove s m then Just piece else Nothing
        _ -> Nothing
  Just _ -> Nothing

isKingMove :: State -> Move -> Bool
isKingMove s m = elem (x2 - x1, y2 - y1)
  [(-1, -1), (-1, 0), (-1, 1), (0, -1), (0, 1), (1, -1), (1, 0), (1, 1)] &&
  not (ofPlayer (pieceAt s (x2, y2)) (toMove s)) where
  (x1, y1) = mfrom m
  (x2, y2) = mto m

isKnightMove :: State -> Move -> Bool
isKnightMove s m = elem (x2 - x1, y2 - y1)
  [(1, 2), (1, -2), (-1, 2), (-1, -2), (2, 1), (2, -1), (-2, 1), (-2, -1)] &&
  not (ofPlayer (pieceAt s (x2, y2)) (toMove s)) where
  (x1, y1) = mfrom m
  (x2, y2) = mto m

isRookMove :: State -> Move -> Bool
isRookMove s m = not (ofPlayer (pieceAt s (x2, y2)) (toMove s))
  && case (x1 == x2, y1 == y2) of
  (True, True) -> False
  (False, False) -> False
  (True, False) -> if y1 < y2 then
    all (== Nothing) (map (pieceAt s) [(x1, i) | i <- [y1 + 1 .. y2 - 1]]) else
    all (== Nothing) (map (pieceAt s) [(x1, i) | i <- [y2 + 1 .. y1 - 1]])
  (False, True) -> if x1 < x2 then
    all (== Nothing) (map (pieceAt s) [(i, y1) | i <- [x1 + 1 .. x2 - 1]]) else
    all (== Nothing) (map (pieceAt s) [(i, y1) | i <- [x2 + 1 .. x1 - 1]])
  where
    (x1, y1) = mfrom m
    (x2, y2) = mto m

isBishopMove :: State -> Move -> Bool
isBishopMove s m = not (ofPlayer (pieceAt s (x2, y2)) (toMove s))
  && case (x2 - x1, y2 - y1) of
  (0, 0) -> False
  (a, b) -> if (a == b && a > 0) then
    all (== Nothing) (map (pieceAt s)
      [(x1+i, y1+i) | i <- [1 .. a - 1]]) else if (a == b && a < 0) then
    all (== Nothing) (map (pieceAt s)
      [(x1-i, y1-i) | i <- [1 .. -a - 1]]) else if (a == -b && a > 0) then
    all (== Nothing) (map (pieceAt s)
      [(x1+i, y1-i) | i <- [1 .. a - 1]]) else if (a == -b && a < 0) then
    all (== Nothing) (map (pieceAt s)
      [(x1-i, y1+i) | i <- [1 .. -a - 1]]) else False
  where
    (x1, y1) = mfrom m
    (x2, y2) = mto m

isQueenMove :: State -> Move -> Bool
isQueenMove s m = isRookMove s m || isBishopMove s m

makeMove :: State -> Move -> Maybe State
makeMove s m = case isLegalMove s m of
  Nothing -> Nothing
  Just piece -> Just $
    State {
      specifics = M.insert (mfrom m) Nothing (
          M.insert (mto m) (Just piece) (specifics s)),
      generals = generals s,
      toMove = nextPlayer (toMove s)
    }

winnerOfGame :: State -> Maybe Player
winnerOfGame s = case filter isRedKing (M.elems (specifics s)) of
  [] -> Just Blue
  _ -> case filter isBlueKing (M.elems (specifics s)) of
    [] -> Just Red
    _ -> Nothing

getRect :: State -> (Integer, Integer) -> (Integer, Integer) -> Response
getRect s (x1, y1) (x2, y2) = if
  (1 <= x2 - x1 && x2 - x1 <= 40 && 1 <= y2 - y1 && y2 - y1 <= 40)
  then ResponseState {
    player = (case winnerOfGame s of
      Nothing -> toMove s
      Just w -> w),
    xdim = x2 - x1 + 1,
    ydim = y2 - y1 + 1,
    pieces = [[pieceAt s (i, j) | j <- [y1 .. y2]] | i <- [x1 .. x2]],
    hasWon = winnerOfGame s
  }
  else ResponseInvalid

data Client = Client {
  connection :: N.Connection,
  clientName :: Maybe String
}

data Game = Game {
  red :: Client,
  --redTime :: Int,
  blue :: Client,
  --blueTime :: Int,
  gameState :: State
}

getGame :: Client -> Client -> IO Game
getGame red blue = do
  return Game {
    red = red,
    blue = blue,
    gameState = initialState
  }

playingAsInGame :: String -> Game -> Maybe Player
playingAsInGame s g = if (clientName (red g) == Just s) then Just Red else
  if (clientName (blue g) == Just s) then Just Blue else Nothing

combineGameRes :: Maybe Player -> Maybe Player -> Maybe Player
combineGameRes (Just p) _ = Just p
combineGameRes _ q = q

playingAs :: ServerState -> String -> Maybe Player
playingAs state string = foldr combineGameRes Nothing
  (map (playingAsInGame string) (games state))

playingInGame :: Client -> Game -> Bool
playingInGame c g = clientName (red g) == clientName c
  || clientName (blue g) == clientName c

playingIn :: ServerState -> Client -> Maybe Game
playingIn state c = case filter (playingInGame c) (games state) of
  [] -> Nothing
  (g : _) -> Just g

data ServerState = ServerState {
  namedClients :: M.Map String Client,
  games :: [Game]
}

allNames :: ServerState -> [String]
allNames state = concatMap (fromName . clientName) (namedClients state) where
  fromName (Just s) = [s]
  fromName Nothing = []

data Request = RequestMove Move | RequestName String |
  RequestAttacking (Integer, Integer) (Integer, Integer) (Integer, Integer) |
  RequestStartGame String
  | RequestState {
  x1 :: Integer,
  y1 :: Integer,
  x2 :: Integer,
  y2 :: Integer
} deriving (Generic, Show)

data Response = ResponseHello String [String] | ResponseInvalid |
  ResponseInvalidName | ResponseInvalidGame |
  ResponseAttacking [(Integer, Integer)] | ResponseState {
  player :: Player,
  xdim :: Integer,
  ydim :: Integer,
  pieces :: [[Maybe Piece]],
  hasWon :: Maybe Player
} deriving (Generic, Show)

isValidName :: Client -> ServerState -> String -> Bool
isValidName client state s = clientName client == Nothing &&
  not (elem s (allNames state)) &&
  all (\c -> C.isAsciiLower c || C.isAsciiUpper c) s &&
  1 <= length s && length s <= 32

sendNames :: ServerState -> IO ()
sendNames s = sequence_ [sendTo c | c <- M.elems (namedClients s)] where
  sendTo c = case clientName c of
    Nothing -> return ()
    Just name -> N.sendTextData (connection c)
      (J.encode (ResponseHello name (allNames s)))

clientLoop :: MVar ServerState -> Client -> IO ()
clientLoop state client = do
  putStrLn "loopin"
  m <- getMessage state client
  putStrLn "received:"
  putStrLn (T.unpack (T.decodeUtf8 m))
  curState <- readMVar state
  case J.decode m of
    Nothing -> do
      N.sendTextData (connection client) (J.encode ResponseInvalid)
      clientLoop state client
    Just (RequestName s) -> if isValidName client curState s then
      do
        let newClient = Client {connection = connection client,
                                clientName = Just s}
        modifyMVar_ state $ \serverState -> return $
          serverState {namedClients =
                       M.insert s newClient (namedClients serverState)}
        serverState <- readMVar state
        sendNames serverState
        clientLoop state newClient
      else do
        N.sendTextData (connection client) (J.encode ResponseInvalidName)
        clientLoop state client
    Just (RequestStartGame s) -> case clientName client of
      Nothing -> do
        N.sendTextData (connection client) (J.encode ResponseInvalidGame)
        clientLoop state client
      Just name -> do
        if s /= name && elem s (allNames curState) &&
            playingAs curState s == Nothing then do
          g <- getGame client (namedClients curState M.! s)
          modifyMVar_ state $ \serverState -> return $
            serverState {games = g : games serverState}
        else do
          N.sendTextData (connection client) (J.encode ResponseInvalidGame)
          clientLoop state client
    Just (RequestState x1 y1 x2 y2) -> case playingIn curState client of
      Just g -> do
        let r = J.encode (getRect (gameState g) (x1, y1) (x2, y2))
        putStrLn "AAA"
        N.sendTextData (connection client) r
      Nothing -> do
        N.sendTextData (connection client) (J.encode ResponseInvalid)
        putStrLn "BBB"
        clientLoop state client
    Just _ -> do
      N.sendTextData (connection client) (J.encode ResponseInvalid)
      clientLoop state client

getMessage :: MVar ServerState -> Client -> IO (BS.ByteString)
getMessage state client = catch
  (N.receiveDataMessage (connection client) >>=
   (return . N.fromDataMessage)) handler where
  handler :: N.ConnectionException -> IO (BS.ByteString)
  handler _ = do
    case (clientName client) of
      Nothing -> return ()
      Just name -> do
        modifyMVar_ state $ \serverState -> return $
          serverState {namedClients = M.delete name (namedClients serverState)}
        serverState <- readMVar state
        sendNames serverState
    return BS.empty

main :: IO ()
main = do
  --putStrLn ((T.unpack . T.decodeUtf8) (J.encode (RequestName "Steve")))
  putStrLn "server started"
  state <- newMVar (ServerState {namedClients = M.empty, games = []})
  N.runServer "192.168.1.99" 6969 $ \pc -> do
    c <- N.acceptRequest pc
    clientLoop state (Client {connection = c, clientName = Nothing})


{-
handleClient :: N.Connection -> State -> IO ()
handleClient c state = do
  m <- N.receive c
  case m of
    N.DataMessage _ _ _ (N.Text s _) -> do
      case J.decode s of
        Nothing -> handleClient c state
        Just (RequestState x1 y1 x2 y2) -> do
          let r = N.Text (J.encode (getRect state (x1, y1) (x2, y2))) Nothing
          N.sendDataMessage c r
          handleClient c state
        Just (RequestMove move) -> case makeMove state move of
          Nothing -> handleClient c state
          Just state2 -> handleClient c state2
        Just (RequestAttacking p p1 p2) -> do
          let r = N.Text (J.encode (listAttacking state p p1 p2)) Nothing
          N.sendDataMessage c r
          handleClient c state
    _ -> handleClient c state

main :: IO ()
main = do
  putStrLn "server online"
  N.runServer "localhost" 8000 $ \pc -> do
    c <- N.acceptRequest pc
    handleClient c initialState
-}
