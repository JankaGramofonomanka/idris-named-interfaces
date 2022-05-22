module Game


import Control.Monad.State
import Control.Monad.Error.Either
import Control.Monad.Error.Interface
import Data.List



-- Points ---------------------------------------------------------------------
Points = Int

addPoints : MonadState Points m => Points -> m ()
addPoints pts = modify (+ pts)

getPoints : MonadState Points m => m Points
getPoints = get


-- Position -------------------------------------------------------------------
Position : Type
Position = (Int, Int)

Direction : Type
Direction = (Int, Int)

move : MonadState Position m => Direction -> m ()
move (dx, dy) = do
  (x, y) <- get
  put (x + dx, y + dy)


-- Item positions -------------------------------------------------------------
Items = List Position

containsItem : MonadState Items m => Position -> m Bool
containsItem pos = do
  items <- get
  if pos `elem` items then pure True else pure False


consumeItem : MonadState Items m => Position -> m ()
consumeItem pos = modify (delete pos)



-- Game mechanics -------------------------------------------------------------
data Input = L | R | U | D
toDirection : Input -> Direction
toDirection L = (-1, 0)
toDirection R = (1, 0)
toDirection U = (0, 1)
toDirection D = (0, -1)

processInput : Monad m
            => (points : MonadState Points m)
            => (position : MonadState Position m)
            => (items : MonadState Items m)
            => Input
            -> m ()
processInput input = do
  move (toDirection input)
  currentPosition <- get @{position}
  foundItem <- containsItem currentPosition
  if foundItem then do
      consumeItem currentPosition
      addPoints 1
    else
      pure ()




-- Display --------------------------------------------------------------------
inRange : Int -> Position -> Position -> Bool
inRange range (x1, y1) (x2, y2) = abs (x1 - x2) < range && abs (y1 - y2) < range


mkRange : Int -> Int -> List Int
mkRange middle radius = rangeFromTo (middle - radius) (middle + radius)

symbol : Items -> Position -> Position -> Char
symbol items playerPos pos
  = if      pos == playerPos then 'O'
    else if pos `elem` items then '*'
    else                          ' '


showBoardCts : Int -> Items -> Position -> List String
showBoardCts range items (x, y) = let
    sortedItems = filter (inRange range (x, y)) (sort items)

    ys = mkRange y (-range)
    xs = mkRange x range

    emptyBoard = map (\_ => xs) ys

    showRow : List Int -> Int -> String
    showRow row yy = pack (map (\xx => symbol items (x, y) (xx, yy)) row)

  in zipWith showRow emptyBoard ys

showBoard range items pos = let
    line = (pack $ map (const '-') [0..2+2*range]) ++ "\n"
    cts = showBoardCts range items pos
    rows = map (\r => "|" ++ r ++ "|\n") cts
  
  in line ++ (concat rows) ++ line


showScore : Int -> String
showScore pts = "Score: " ++ show pts

displayGame  : HasIO m
            => (points : MonadState Points m)
            => (position : MonadState Position m)
            => (items : MonadState Items m)
            => Int
            -> m ()
displayGame range = do
  pts <- get @{points}
  putStrLn $ showScore pts
  items <- get @{items}
  pos <- get @{position}
  putStrLn (showBoard range items pos)


showLine : HasIO m => Int -> m ()
showLine range = putStrLn (pack $ map (const '-') [0..2+2*range])

-- UI -------------------------------------------------------------------------
readInput : (HasIO m, MonadError () m) => m Input
readInput = do
  putStrLn "What's your next move?"
  s <- getLine
  case s of
    "a" => pure L
    "d" => pure R
    "w" => pure U
    "s" => pure D
    "q" => throwError ()
    _   => putStrLn "Invalid input" >> readInput
  


-- `MonadState` implementations -----------------------------------------------
implementation [first] (tuple : MonadState (a, b, c) m) => MonadState a m where
  get = fst <$> get @{tuple}
  put x = do
    (_, y, z) <- get @{tuple}
    put (x, y, z) @{tuple}

implementation [second] (tuple : MonadState (a, b, c) m) => MonadState b m where
  get = (fst . snd) <$> get @{tuple}
  put y = do
    (x, _, z) <- get @{tuple}
    put (x, y, z) @{tuple}

implementation [third] (tuple : MonadState (a, b, c) m) => MonadState c m where
  get = (snd . snd) <$> get @{tuple}
  put z = do
    (x, y, _) <- get @{tuple}
    put (x, y, z) @{tuple}


-- Full Game ------------------------------------------------------------------
playAbs  : HasIO m
        => Monad m
        => MonadError () m
        => MonadState Points m
        => MonadState Position m
        => MonadState Items m
        => Int
        -> m ()
playAbs displayRange = do
  showLine displayRange
  displayGame displayRange

  input <- readInput
  processInput input
  playAbs displayRange

GameState: Type
GameState = (Int, Position, Items)

GameM = EitherT () (StateT GameState IO)



{- Idris is confused here
play1 : Int -> GameM ()
play1 = playAbs @{_} @{_} @{_} @{first} @{second} @{third}

play2 : Int -> GameM ()
play2 displayRange = do
  showLine displayRange
  displayGame displayRange @{io} @{first} @{second} @{third}

  input <- readInput
  processInput input @{monad} @{first} @{second} @{third}
  play2 displayRange
-}

playProxy  : (io    : HasIO m)
          => (monad : Monad m)
          => (error : MonadError () m)
          => (state : MonadState GameState m)
          => Int
          -> m ()
playProxy displayRange = do
  showLine displayRange
  displayGame displayRange @{io} @{first} @{second} @{third}

  input <- readInput
  processInput input @{monad} @{first} @{second} @{third}
  playProxy displayRange


playProxy1 : (io    : HasIO m)
    => (monad : Monad m)
    => (error : MonadError () m)
    => (state : MonadState GameState m)
    => Int
    -> m ()
playProxy1 = playAbs @{io} @{monad} @{error} @{first} @{second} @{third}


play : Int -> GameM ()
play = playProxy

game : Int -> Items -> IO ()
game displayRange items = do
  _ <- execStateT (0, (0, 0), items) $ runEitherT (play displayRange)
  putStrLn "End of Game"



-- Demo -----------------------------------------------------------------------
exampleItems : Items
exampleItems = [(1, 1), (2, 3), (2, 2), (-3, 2), (-2, -3)]

demo : IO ()
demo = game 15 exampleItems
