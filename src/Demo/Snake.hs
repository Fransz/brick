module Demo.Snake 
    ( SnakeState (..)
    , SnakeMove (..)
    , Pos
    , initialState
    , move
    , addMove
    , newSnakeState
    ) where

import Lens.Micro ((%~), (&))
import Linear.V2 (V2 (..), _x, _y)

type Pos = V2 Int

data SnakeState = SnakeState
  { rows :: Int,
    cols :: Int,
    snake :: [Pos],
    apple :: Pos,
    direction :: SnakeMove,
    counter :: Int
  }
  deriving (Show)

data SnakeMove = SnakeUp | SnakeDown | SnakeLeft | SnakeRight deriving (Show, Eq)

initialState :: SnakeState
initialState = SnakeState 50 50 [V2 2 6, V2 3 6, V2 4 6, V2 4 7, V2 4 8] (V2 40 6) SnakeRight 0

tick :: Int -> SnakeState -> SnakeState
tick i s = s {counter = counter s + i}

move :: SnakeState -> SnakeState
move s =
  let a = apple s
      (x, y) = moveToDirection $ direction s
      a' = a & _x %~ (\x' -> (x' + x) `mod` cols s)
      a'' = a' & _y %~ (\y' -> (y' + y) `mod` rows s)
   in s {apple = a''}

moveToDirection :: SnakeMove -> (Int, Int)
moveToDirection SnakeRight = (1,0)
moveToDirection SnakeLeft = (-1,0)
moveToDirection SnakeUp = (0,-1)
moveToDirection SnakeDown = (0,1)

addMove :: SnakeState -> SnakeMove -> SnakeState
addMove s m | m `elem` [SnakeUp, SnakeDown] && cur `elem` [SnakeUp, SnakeDown] = s
            | m `elem` [SnakeUp, SnakeDown] = s {direction = m}
            | m `elem` [SnakeLeft, SnakeRight] &&  cur `elem` [SnakeLeft, SnakeRight] = s 
            | m `elem` [SnakeLeft, SnakeRight] = s {direction = m}
    where cur = direction s

newSnakeState :: SnakeState -> SnakeState
newSnakeState = tick 1 . move 
