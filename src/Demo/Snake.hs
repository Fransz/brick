module Demo.Snake (SnakeState(..),CounterEvent(..),move,Pos,incCounter,initialState)
where

import Linear.V2 (V2(..), _x, _y)
import Lens.Micro ((%~), (&))

newtype CounterEvent = Counter Int

type Pos = V2 Int

data SnakeState = SnakeState {
  rows :: Int,
  cols :: Int,
  snake :: [Pos],
  apple :: Pos,
  moves :: [(Int, Int)],
  counter :: Int
} deriving (Show)

initialState :: SnakeState
initialState = SnakeState 50 50 [V2 2 6, V2 3 6, V2 4 6, V2 4 7, V2 4 8] (V2 40 6) [] 0

incCounter :: SnakeState -> Int -> SnakeState
incCounter s i = s { counter = counter s + i }

move :: SnakeState -> (Int, Int) -> SnakeState
move s (x, y) = let a = apple s
                    a' = a & _x %~ (\x' -> (x'+ x) `mod` cols s)
                    a'' = a' & _y %~ (\y' -> (y'+ y) `mod` rows s)
                in s { apple = a'' }
