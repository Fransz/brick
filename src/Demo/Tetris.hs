{-# LANGUAGE TupleSections #-}

module Demo.Tetris
  ( Game (..),
    Block (..),
    TetrisDirection (..),
    initGame,
    moveGameM,
    tickGameM,
    freeFallM,
    posNameMap,
    changeSpeed,
    Tetris,
    initialGame,
  )
where

import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TVar (TVar, readTVarIO, writeTVar)
import Control.Monad (unless, void, when)
import Control.Monad.Trans.State (State, get, put)
import Data.List as List (groupBy, sortOn)
import qualified Data.Map as Map (Map, fromList)
import qualified Data.Ord (Down (..))
import Lens.Micro ((^.))
import Linear.V2 (V2 (..), perp, _x, _y)
import qualified System.Random as Random (Random (randomR), StdGen, newStdGen)

type Tetris = State Game

type Pos = V2 Int

data TetrisDirection = TetrisLeft | TetrisRight | TetrisUp | TetrisDown deriving (Show, Eq)

data BlockStatus = Moving | Dropped deriving (Show, Eq)

data Block = Block
  { pos :: Pos,
    poss :: [Pos],
    name :: String,
    status :: BlockStatus
  }
  deriving (Show)

data Brick = Brick
  { brPos :: Pos,
    brName :: String
  }
  deriving (Show)

instance Eq Brick where
  (==) b1 b2 = brPos b1 == brPos b2

iBlock = Block {pos = V2 10 0, poss = [V2 0 (-2), V2 0 (-1), V2 0 0, V2 0 1], name = "iblock", status = Moving}

oBlock = Block {pos = V2 10 0, poss = [V2 0 (-1), V2 1 (-1), V2 0 0, V2 1 0], name = "oblock", status = Moving}

tBlock = Block {pos = V2 10 0, poss = [V2 (-1) (-1), V2 0 (-1), V2 0 0, V2 1 (-1)], name = "tblock", status = Moving}

sBlock = Block {pos = V2 10 0, poss = [V2 0 (-1), V2 1 (-1), V2 (-1) 0, V2 0 0], name = "sblock", status = Moving}

zBlock = Block {pos = V2 10 0, poss = [V2 (-1) (-1), V2 0 (-1), V2 0 0, V2 1 0], name = "zblock", status = Moving}

lBlock = Block {pos = V2 10 0, poss = [V2 0 (-2), V2 0 (-1), V2 0 0, V2 1 0], name = "lblock", status = Moving}

jBlock = Block {pos = V2 10 0, poss = [V2 0 (-2), V2 0 (-1), V2 0 0, V2 (-1) 0], name = "jblock", status = Moving}

testWall = testRow1 ++ testRow2 ++ testRow3 ++ testRow4 ++ testRow5 ++ testRow6

testRow1 = map (`Brick` "oblock") $ (take 8 . makeRow $ V2 0 19) ++ (take 7 . makeRow $ V2 9 19)

testRow2 = map (`Brick` "oblock") $ (take 3 . makeRow $ V2 0 18) ++ (take 3 . makeRow $ V2 5 18) ++ (take 7 . makeRow $ V2 9 18)

testRow3 = map (`Brick` "oblock") $ (take 8 . makeRow $ V2 0 17) ++ (take 7 . makeRow $ V2 9 17)

testRow4 = map (`Brick` "oblock") $ (take 8 . makeRow $ V2 0 16) ++ (take 3 . makeRow $ V2 9 16) ++ (take 3 . makeRow $ V2 13 16)

testRow5 = map (`Brick` "oblock") $ (take 4 . makeRow $ V2 2 15) ++ (take 4 . makeRow $ V2 12 15)

testRow6 = map (`Brick` "oblock") $ (take 2 . makeRow $ V2 2 14) ++ (take 1 . makeRow $ V2 12 14)

makeRow = iterate (\v -> V2 (v ^. _x + 1) (v ^. _y))

data Game = Game
  { cols :: Int,
    rows :: Int,
    block :: Block,
    wall :: [Brick],
    gameover :: Bool,
    counter :: Int,
    score :: Int,
    speed :: Int,
    gen :: Random.StdGen,
    delay :: TVar Int
  }

instance Show Game where
  show g =
    show (cols g)
      ++ show (rows g)
      ++ show (block g)
      ++ show (wall g)
      ++ show (gameover g)
      ++ show (counter g)
      ++ show (score g)

initialGame :: Game
initialGame =
  Game
    { cols = 16,
      rows = 20,
      block = iBlock,
      wall = testWall,
      gameover = False,
      counter = 0,
      score = 0,
      speed = 0,
      gen = undefined,
      delay = undefined
    }

initialGame' :: Tetris Game
initialGame' = return initialGame

moveGameM :: TetrisDirection -> Tetris ()
moveGameM dir = do
  g <- get
  put $ moveGame' dir g
  return ()

--
-- Drop the block of a game until it is Dropped in the wall.
freeFallM :: Tetris ()
freeFallM = do
  g <- get
  unless (status (block g) == Dropped) (moveGameM TetrisDown >> freeFallM)

--
-- periodic action.
tickGameM :: Tetris ()
tickGameM = do
  g <- get
  if status (block g) == Dropped
    then buildWallM >> collapseWallM >> newBlockM
    else moveGameM TetrisDown

-- | isGameOver g = return $ g {gameover = True}

--
-- Move the block of a game; keep the current if the move is invalid; mark as Dropped as into the wall.
moveGame' :: TetrisDirection -> Game -> Game
moveGame' dir game = game {block = setDropped . keepFromWall . keepInBounds . moveBlock dir $ curBlock}
  where
    curBlock = block game
    keepInBounds b = if dir /= TetrisDown && not (inBounds b 0 (cols game)) then curBlock else b
    keepFromWall b = if dir /= TetrisDown && inWall b (wall game ++ ground game) then curBlock else b
    setDropped b = if dir == TetrisDown && inWall b (wall game ++ ground game) then curBlock {status = Dropped} else b

--
-- Move a block.
moveBlock :: TetrisDirection -> Block -> Block
moveBlock d b = case d of
  TetrisLeft -> moveCenter b (V2 (-1) 0)
  TetrisRight -> moveCenter b (V2 1 0)
  TetrisDown -> moveCenter b (V2 0 1)
  TetrisUp -> rotate b

--
-- Move the center position of a block.
moveCenter :: Block -> V2 Int -> Block
moveCenter b d = b {pos = pos b + d}

--
-- rotate a block.
rotate :: Block -> Block
rotate b = b {poss = map perp $ poss b}

--
-- add a block to the wall
buildWallM :: Tetris ()
buildWallM = do
  g <- get
  let toBricks bl = map (\p -> Brick (p + pos bl) (name bl)) (poss bl)
  when (status (block g) == Dropped) $ put (g {wall = wall g ++ toBricks (block g)})

--
-- Calculate the ground of the game. I.e. the first invisable row.
ground :: Game -> [Brick]
ground game = map (`Brick` "") $ take (cols game) . iterate (\v -> V2 (v ^. _x + 1) (rows game)) $ V2 0 (rows game)

--
-- Check if a block is in the field.
inBounds :: Block -> Int -> Int -> Bool
inBounds b min max = all (>= min) xs && all (< max) xs
  where
    xs = map ((^. _x) . (+ pos b)) (poss b)

--
-- Check if a block is in the wall.
inWall :: Block -> [Brick] -> Bool
inWall b w = any ((`elem` map brPos w) . (+ pos b)) (poss b)

--
-- ticker.
tick :: Int -> Game -> Game
tick i g = g {counter = counter g + i}

--
-- Create a new random block until it is inbounds
newBlockM :: Tetris ()
newBlockM = do
  g <- get
  let blocks = [iBlock, oBlock, tBlock, sBlock, zBlock, lBlock, jBlock]
      (pos, gen') = Random.randomR (V2 0 0, V2 (cols g - 1) 0) (gen g)
      (idx, gen'') = Random.randomR (0, 6) gen'
      block = (blocks !! idx) {pos = pos}
  if inBounds block 0 (cols g)
    then put $ g {block = block, gen = gen''}
    else (put $ g {gen = gen''}) >> newBlockM

--
-- Check if the game is over.
isGameOver :: Game -> Bool
isGameOver g = not (null w) && ((^. _y) . brPos . head $ w) <= 0
  where
    w = sortOn ((^. _y) . brPos) $ wall g

--
-- Collapse wall
collapseWallM :: Tetris ()
collapseWallM = do
  g <- get
  let wall' = collapseWall' (cols g) (groupWall $ wall g)
      dRows = (length (wall g) - length wall') `quot` cols g
      score' = 10 ^ dRows + score g
      speed' = abs $ speed g - truncate (fromIntegral (dRows * speed g) / 20)
  put $ g {wall = wall', score = score', speed = speed'}

--
-- Remove full rows from the wall.
collapseWall' :: Int -> [[Brick]] -> [Brick]
collapseWall' full l
  | [] <- l = []
  | (r : rs) <- l, length r == full = collapseWall' full (map (map dropBrick) rs)
  | (r : rs) <- l = r ++ collapseWall' full rs
  where
    dropBrick br = br {brPos = brPos br + V2 0 1}

--
-- grouped bricks by row number. Highest rowNumber first.
groupWall :: [Brick] -> [[Brick]]
groupWall w =
  let sortWall = sortOn (Data.Ord.Down . (^. _y) . brPos) w
   in groupBy (\b1 b2 -> brPos b1 ^. _y == brPos b2 ^. _y) sortWall

--
-- Change the speed and the delay of the game
changeSpeed :: Game -> (Int -> Int -> Int) -> IO Game
changeSpeed g (+/-) = do
  d <- readTVarIO (delay g)
  atomically $ writeTVar (delay g) $ (+/-) d 10000
  return g {speed = (+/-) d 10000}

--
-- Set the delay of the game with the speed.
setDelay :: Game -> IO Game
setDelay g = do
  atomically $ writeTVar (delay g) (speed g)
  return g

--
-- Map of all blocks, all positions with the blocks attr.
posNameMap :: Game -> Map.Map Pos String
posNameMap g = Map.fromList (posNameTpls (block g) ++ posNameWall (wall g))

--
-- List off absolute positions of a block, in a tuple with the blocks attrName
posNameTpls :: Block -> [(Pos, String)]
posNameTpls b = map ((,name b) . (+ pos b)) (poss b)

--
-- List off absolute positions of bricks, in a tuple with the bricks attrName
posNameWall :: [Brick] -> [(Pos, String)]
posNameWall = map (\b -> (brPos b, brName b))

--
-- Initialize the game. I.e. create a stdGen
initGame :: TVar Int -> IO Game
initGame delay = do
  g <- Random.newStdGen
  speed <- readTVarIO delay
  return $ initialGame {gen = g, delay = delay, speed = speed}
