module Demo.Snake
    ( SnakeState (..)
    , SnakeMove (..)
    , Pos
    , initState
    , addMove
    , newSnakeState
    ) where

import Lens.Micro ((^.))
import Linear.V2 (V2 (..), _x, _y)
import System.Random (randomRs, newStdGen)

type Pos = V2 Int

data SnakeState = SnakeState
  { rows :: Int
  , cols :: Int
  , snake :: [Pos]
  , apple :: Pos
  , direction :: SnakeMove
  , gameover :: Bool
  , counter :: Int
  , appleCount :: Int
  , rndApples :: [V2 Int]
  }
  deriving (Show)

data SnakeMove = SnakeUp | SnakeDown | SnakeLeft | SnakeRight deriving (Show, Eq)

initialState :: SnakeState
initialState = SnakeState
    { cols = 50
    , rows = 50
    , snake = [V2 6 6]
    , apple = V2 44 6
    , direction = SnakeRight
    , gameover = False
    , counter = 0
    , appleCount = 0
    , rndApples = []
}

newSnakeState :: SnakeState -> SnakeState
newSnakeState s | gameover s = s
                | otherwise = tick 1 $ move s

tick :: Int -> SnakeState -> SnakeState
tick i s = s {counter = counter s + i}

move :: SnakeState -> SnakeState
move s = (eatApple h . growSnake h . isCrash h) s
    where h = nextHead s
         
isCrash :: Pos -> SnakeState -> SnakeState
isCrash p s | p `elem` snake s = s { gameover = True }
            | otherwise = s

growSnake :: Pos -> SnakeState -> SnakeState
growSnake p s | gameover s = s
              | otherwise = s { snake = p : snake s }

eatApple :: Pos -> SnakeState -> SnakeState
eatApple p s 
  | gameover s = s
  | apple s == p = let (a', rndApples') = nextApple (snake s) (rndApples s)
                    in s { apple = a', rndApples = rndApples', appleCount = appleCount s + 1 }
  | otherwise = s { snake = init (snake s) }

nextApple :: [Pos] -> [Pos] -> (Pos, [Pos])
nextApple ps rps | head rps `elem` ps = nextApple ps (tail rps)
                 | otherwise = (head rps, tail rps)

nextHead :: SnakeState -> Pos
nextHead s = V2 ((dx + x) `mod` cols s) ((dy + y) `mod` rows s)
    where p = head $ snake s 
          (x, y) = (p ^._x, p ^._y)
          (dx, dy) = toDelta $ direction s

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


initState :: IO SnakeState
initState = do g <- newStdGen
               return initialState { rndApples = randomRs (V2 0 0, V2 (cols initialState - 1) (rows initialState - 1)) g}

