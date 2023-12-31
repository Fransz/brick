{-# LANGUAGE TupleSections #-}

module Demo.Tetris
  ( Tetris (..),
    Block (..),
    TetrisDirection (..),
    initialTetris,
    moveTetrisM,
    tickTetrisM,
    freeFallM,
    posNameMap,
    TetrisS,
  )
where

import Control.Monad (unless, when)
import Control.Monad.Trans.State (State, get, put)
import Data.List as List (groupBy, sortOn)
import qualified Data.Map as Map (Map, fromList)
import qualified Data.Ord (Down (..))
import Lens.Micro ((^.))
import Linear.V2 (V2 (..), perp, _x, _y)
import qualified System.Random as Random (Random (randomR), StdGen)

type TetrisS = State Tetris

type Pos = V2 Int

data TetrisDirection = TetrisLeft | TetrisRight | TetrisUp | TetrisDown deriving (Show, Eq)

data BlockStatus = Moving | Dropped deriving (Show, Eq)

data Block = Block
  { blPos :: Pos,
    blPosRs :: [Pos],
    blName :: String,
    blStatus :: BlockStatus
  }
  deriving (Show)

data Brick = Brick
  { brPos :: Pos,
    brName :: String
  }
  deriving (Show)

instance Eq Brick where
  (==) b1 b2 = brPos b1 == brPos b2

iBlock = Block {blPos = V2 10 0, blPosRs = [V2 0 (-2), V2 0 (-1), V2 0 0, V2 0 1], blName = "iblock", blStatus = Moving}

oBlock = Block {blPos = V2 10 0, blPosRs = [V2 0 (-1), V2 1 (-1), V2 0 0, V2 1 0], blName = "oblock", blStatus = Moving}

tBlock = Block {blPos = V2 10 0, blPosRs = [V2 (-1) (-1), V2 0 (-1), V2 0 0, V2 1 (-1)], blName = "tblock", blStatus = Moving}

sBlock = Block {blPos = V2 10 0, blPosRs = [V2 0 (-1), V2 1 (-1), V2 (-1) 0, V2 0 0], blName = "sblock", blStatus = Moving}

zBlock = Block {blPos = V2 10 0, blPosRs = [V2 (-1) (-1), V2 0 (-1), V2 0 0, V2 1 0], blName = "zblock", blStatus = Moving}

lBlock = Block {blPos = V2 10 0, blPosRs = [V2 0 (-2), V2 0 (-1), V2 0 0, V2 1 0], blName = "lblock", blStatus = Moving}

jBlock = Block {blPos = V2 10 0, blPosRs = [V2 0 (-2), V2 0 (-1), V2 0 0, V2 (-1) 0], blName = "jblock", blStatus = Moving}

testWall = testRow1 ++ testRow2 ++ testRow3 ++ testRow4 ++ testRow5 ++ testRow6

testRow1 = map (`Brick` "oblock") $ (take 8 . makeRow $ V2 0 19) ++ (take 7 . makeRow $ V2 9 19)

testRow2 = map (`Brick` "oblock") $ (take 3 . makeRow $ V2 0 18) ++ (take 3 . makeRow $ V2 5 18) ++ (take 7 . makeRow $ V2 9 18)

testRow3 = map (`Brick` "oblock") $ (take 8 . makeRow $ V2 0 17) ++ (take 7 . makeRow $ V2 9 17)

testRow4 = map (`Brick` "oblock") $ (take 8 . makeRow $ V2 0 16) ++ (take 3 . makeRow $ V2 9 16) ++ (take 3 . makeRow $ V2 13 16)

testRow5 = map (`Brick` "oblock") $ (take 4 . makeRow $ V2 2 15) ++ (take 4 . makeRow $ V2 12 15)

testRow6 = map (`Brick` "oblock") $ (take 2 . makeRow $ V2 2 14) ++ (take 1 . makeRow $ V2 12 14)

makeRow = iterate (\v -> V2 (v ^. _x + 1) (v ^. _y))

data Tetris = Tetris
  { cols :: Int,
    rows :: Int,
    block :: Block,
    wall :: [Brick],
    gameOver :: Bool,
    counter :: Int,
    score :: Int,
    speed :: Int,
    gen :: Random.StdGen
  }

instance Show Tetris where
  show g =
    show (cols g)
      ++ show (rows g)
      ++ show (block g)
      ++ show (wall g)
      ++ show (gameOver g)
      ++ show (counter g)
      ++ show (score g)

initialTetris :: Tetris
initialTetris =
  Tetris
    { cols = 17,
      rows = 25,
      block = iBlock,
      wall = [],
      gameOver = False,
      counter = 0,
      score = 0,
      speed = 0,
      gen = undefined
    }

moveTetrisM :: TetrisDirection -> TetrisS ()
moveTetrisM dir = do
  g <- get
  put $ moveTetris' dir g
  return ()

--
-- Drop the block of a game until it is Dropped in the wall.
freeFallM :: TetrisS ()
freeFallM = do
  g <- get
  unless (blStatus (block g) == Dropped) (moveTetrisM TetrisDown >> freeFallM)

--
-- periodic action.
tickTetrisM :: TetrisS ()
tickTetrisM = do
  g <- get
  if blStatus (block g) == Dropped
    then buildWallM >> collapseWallM >> newBlockM >> gameOverM
    else moveTetrisM TetrisDown

--
-- Move the block of a game; keep the current if the move is invalid; mark as Dropped as into the wall.
moveTetris' :: TetrisDirection -> Tetris -> Tetris
moveTetris' dir game = game {block = setDropped . keepFromWall . keepInBounds (0, cols game - 1) . moveBlock dir $ curBlock}
  where
    curBlock = block game
    keepFromWall b = if dir /= TetrisDown && inWall b (wall game ++ ground game) then curBlock else b
    setDropped b = if dir == TetrisDown && inWall b (wall game ++ ground game) then curBlock {blStatus = Dropped} else b

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
moveCenter b d = b {blPos = blPos b + d}

--
-- rotate a block.
rotate :: Block -> Block
rotate b = b {blPosRs = map perp $ blPosRs b}

--
-- check if the blocks xs are greater then min
isInLBound :: Int -> Block -> Bool
isInLBound min b = all ((>= min) . (^. _x)) $ blPosAs b

--
-- check if the blocks xs are smaller then max
isInRBound :: Int -> Block -> Bool
isInRBound max b = all ((<= max) . (^. _x)) $ blPosAs b

--
-- Move the block right if it is off the field on the left
keepInLeftBound :: Int -> Block -> Block
keepInLeftBound min b
  | not $ isInLBound min b = keepInLeftBound min $ moveBlock TetrisRight b
  | otherwise = b

--
-- Move the block left if it is off the field on the right
keepInRightBound :: Int -> Block -> Block
keepInRightBound max b
  | not $ isInRBound max b = keepInRightBound max $ moveBlock TetrisLeft b
  | otherwise = b

--
-- Move the block if it is off the field (if possible)
keepInBounds :: (Int, Int) -> Block -> Block
keepInBounds (min, max) b
  | max - min <= (length . blPosRs) b = b -- the block doesnt fit between min and max
  | otherwise = keepInRightBound max $ keepInLeftBound min b

--
-- Calculate the ground of the game. I.e. the first invisable row.
ground :: Tetris -> [Brick]
ground game = map (`Brick` "") $ take (cols game) . iterate (\v -> V2 (v ^. _x + 1) (rows game)) $ V2 0 (rows game)

--
-- Check if a block is in the wall.
inWall :: Block -> [Brick] -> Bool
inWall b w = any (`elem` map brPos w) $ blPosAs b

--
-- Turn a block into bricks.
toBricks :: Block -> [Brick]
toBricks bl = map (\p -> Brick p (blName bl)) $ blPosAs bl

--
-- add a block to the wall
buildWallM :: TetrisS ()
buildWallM = do
  g <- get
  when (blStatus (block g) == Dropped) $ put (g {wall = wall g ++ toBricks (block g)})

--
-- Collapse wall
collapseWallM :: TetrisS ()
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
  | (r : rs) <- l, length r == full = collapseWall' full $ dropBricks rs
  | (r : rs) <- l = r ++ collapseWall' full rs
  where
    dropBricks brs = (map . map) dropBrick brs
    dropBrick br = br {brPos = brPos br + V2 0 1}

--
-- grouped bricks by row number. Highest rowNumber first.
groupWall :: [Brick] -> [[Brick]]
groupWall w =
  let sortWall = sortOn (Data.Ord.Down . (^. _y) . brPos) w
   in groupBy (\b1 b2 -> brPos b1 ^. _y == brPos b2 ^. _y) sortWall

--
-- All absolute coordinates of a block
blPosAs :: Block -> [Pos]
blPosAs b = map (+ blPos b) (blPosRs b)

--
-- Create a new random block until it is inbounds
newBlockM :: TetrisS ()
newBlockM = do
  g <- get
  let blocks = [iBlock, oBlock, tBlock, sBlock, zBlock, lBlock, jBlock]
      (blPos, gen') = Random.randomR (V2 0 0, V2 (cols g - 1) 0) (gen g)
      (idx, gen'') = Random.randomR (0, 6) gen'
      block = (blocks !! idx) {blPos = blPos}
      block' = keepInBounds (0, cols g - 1) block
  put $ g {block = block', gen = gen''}

--
-- ticker.
tick :: Int -> Tetris -> Tetris
tick i g = g {counter = counter g + i}

--
-- Check if the game is over.
gameOverM :: TetrisS ()
gameOverM = do
  g <- get
  when (any ((== 0) . (^. _y) . brPos) $ wall g) (put g {gameOver = True})

--
-- Map of all blocks, all positions with the blocks attr.
posNameMap :: Tetris -> Map.Map Pos String
posNameMap g = Map.fromList (posNameTpls (block g) ++ posNameWall (wall g))

--
-- List off absolute positions of a block, in a tuple with the blocks attrName
posNameTpls :: Block -> [(Pos, String)]
posNameTpls b = map (,blName b) $ blPosAs b

--
-- List off absolute positions of bricks, in a tuple with the bricks attrName
posNameWall :: [Brick] -> [(Pos, String)]
posNameWall = map (\b -> (brPos b, brName b))
