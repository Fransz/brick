module Demo.Snake
    ( SnakeState (..)
    , SnakeMove (..)
    , Pos
    , initialState
    , addMove
    , newSnakeState
    ) where

import Lens.Micro ((^.))
import Linear.V2 (V2 (..), _x, _y)

type Pos = V2 Int

data SnakeState = SnakeState
  { rows :: Int
  , cols :: Int
  , snake :: [Pos]
  , apple :: Pos
  , direction :: SnakeMove
  , gameover :: Bool
  , counter :: Int
  , apples :: Int
  }
  deriving (Show)

data SnakeMove = SnakeUp | SnakeDown | SnakeLeft | SnakeRight deriving (Show, Eq)

initialState :: SnakeState
initialState = SnakeState
    { cols = 50
    , rows = 50
    , snake = [V2 6 6, V2 7 6, V2 8 6, V2 9 6, V2 10 6, V2 11 6, V2 12 6, V2 13 6, V2 14 6, V2 15 6, V2 16 6, V2 17 6, V2 18 6, V2 19 6, V2 20 6, V2 21 6, V2 22 6]
    , apple = V2 44 6
    , direction = SnakeLeft
    , gameover = False
    , counter = 0
    , apples = 0
}

tick :: Int -> SnakeState -> SnakeState
tick i s = s {counter = counter s + i}

{-
 - moveApple :: SnakeState -> SnakeState
 - moveApple s =
 -   let a = apple s
 -       (x, y) = toDelta $ direction s
 -       a' = a & _x %~ (\x' -> (x' + x) `mod` cols s)
 -       a'' = a' & _y %~ (\y' -> (y' + y) `mod` rows s)
 -    in s {apple = a''}
 -}

move :: SnakeState -> SnakeState
move s = let nh = nextHead s
         in (eatApple nh . growSnake nh . isCrash nh) s

growSnake :: Pos -> SnakeState -> SnakeState
growSnake p s | gameover s = s
              | otherwise = s { snake = p : snake s }

eatApple :: Pos -> SnakeState -> SnakeState
eatApple p s | gameover s = s
  | apple s == p = s { apple = V2 6 44, apples = apples s + 1 }
             | otherwise = s { snake = init (snake s) }

isCrash :: Pos -> SnakeState -> SnakeState
isCrash p s | p `elem` snake s = s { gameover = True }
            | otherwise = s

nextHead :: SnakeState -> Pos
nextHead s = let p = head $ snake s 
                 (dx, dy) = toDelta $ direction s
                 (x, y) = (p ^._x, p ^._y)
              in V2 ((dx + x) `mod` cols s) ((dy + y) `mod` rows s)

toDelta :: SnakeMove -> (Int, Int)
toDelta SnakeRight = (1,0)
toDelta SnakeLeft = (-1,0)
toDelta SnakeUp = (0,-1)
toDelta SnakeDown = (0,1)

addMove :: SnakeState -> SnakeMove -> SnakeState
addMove s m | m `elem` [SnakeUp, SnakeDown] && cur `elem` [SnakeUp, SnakeDown] = s
            | m `elem` [SnakeUp, SnakeDown] = s {direction = m}
            | m `elem` [SnakeLeft, SnakeRight] &&  cur `elem` [SnakeLeft, SnakeRight] = s
            | m `elem` [SnakeLeft, SnakeRight] = s {direction = m}
    where cur = direction s

newSnakeState :: SnakeState -> SnakeState
newSnakeState s | gameover s = s
                | otherwise = tick 1 $ move s
