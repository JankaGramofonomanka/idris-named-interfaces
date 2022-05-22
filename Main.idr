module Main

import Control.Monad.State


implementation [left] (tuple : MonadState (a, b) m) => MonadState a m where
  get = fst <$> get @{tuple}
  put x = do
    (_, y) <- get @{tuple}
    put (x, y) @{tuple}

implementation [right] (tuple : MonadState (a, b) m) => MonadState b m where
  get = snd <$> get @{tuple}
  put y = do
    (x, _) <- get @{tuple}
    put (x, y) @{tuple}


implementation [stateLeft] MonadState a (State (a, b)) where
  get = fst <$> get {stateType = (a, b)}
  put x = do
    (_, y) <- get {stateType = (a, b)}
    put (x, y) {stateType = (a, b)}

implementation [stateRight] MonadState b (State (a, b)) where
  get = snd <$> get {stateType = (a, b)}
  put y = do
    (x, _) <- get {stateType = (a, b)}
    put (x, y) {stateType = (a, b)}


incr : MonadState Int m => m ()
incr = do
  s <- get
  put (s + 1)


log : MonadState (List String) m => String -> m ()
log msg = do
  logs <- get
  put (msg :: logs)


CounterAndLogger = State (Int, List String)

getLeft : CounterAndLogger Int
getLeft = do
  (l, _) <- get
  pure l

getLeft1 : CounterAndLogger Int
getLeft1 = get @{left}


processInput : Int -> CounterAndLogger ()
processInput n = do
  if n < 0 then
      log ("input less than zero: " ++ show n) @{right}
    else
      incr @{left}

--processInputAbs : ((counter : MonadState Int m), (logger : MonadState (List String) m)) => m ()
processInputAbs : (counter : MonadState Int m) => (logger : MonadState (List String) m) => Int -> m ()
processInputAbs n = do
  if n < 0 then
      log ("input less than zero: " ++ show n) @{logger}
    else
      incr @{counter}



processInputs1 : List Int -> CounterAndLogger ()
processInputs1 inputs = do
  _ <- traverse processInput inputs
  pure ()

processInputs2 : List Int -> CounterAndLogger ()
processInputs2 inputs = do
  _ <- traverse (processInputAbs @{left} @{right}) inputs
  pure ()

processInputs3 : List Int -> CounterAndLogger ()
processInputs3 inputs = do
  _ <- traverse (processInputAbs @{stateLeft} @{stateRight}) inputs
  pure ()

{-
processInputsAbs : (counter : MonadState Int m)
                => (logger : MonadState (List String) m)
                => List Int
                -> m ()
processInputsAbs inputs = do
  _ <- traverse (processInputAbs @{counter} @{logger}) inputs
  pure ()
-}

perform : CounterAndLogger () -> (Int, List String)
perform computation = execState (0, []) computation


example1 : (Int, List String)
example1 = perform (processInputs1 [1, -2, 3, -4, 5, -6])

example2 : (Int, List String)
example2 = perform (processInputs2 [1, -2, 3, -4, 5, -6])

example3 : (Int, List String)
example3 = perform (processInputs2 [1, -2, 3, -4, 5, -6])
