{-# LANGUAGE TupleSections #-}

module Demo.Tetris
  ( Game (..),
    Block (..),
    TetrisDirection (..),
    initGame,
    moveGame,
    tickGame,
    freeFall,
    posNameMap,
  )
where

import Data.List as List (groupBy, nubBy, sortOn, (\\))
import qualified Data.Map as Map (Map, fromList)
import qualified Data.Ord (Down (..))
import Lens.Micro ((^.))
import Linear.V2 (V2 (..), perp, _x, _y)
import qualified System.Random as Random (Random (randomR), StdGen, newStdGen)

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

iBlock = Block {pos = V2 0 0, poss = [V2 0 (-2), V2 0 (-1), V2 0 0, V2 0 1], name = "iblock", status = Moving}

oBlock = Block {pos = V2 0 0, poss = [V2 0 (-1), V2 1 (-1), V2 0 0, V2 1 0], name = "oblock", status = Moving}

tBlock = Block {pos = V2 0 0, poss = [V2 (-1) (-1), V2 0 (-1), V2 0 0, V2 1 (-1)], name = "tblock", status = Moving}

sBlock = Block {pos = V2 0 0, poss = [V2 0 (-1), V2 1 (-1), V2 (-1) 0, V2 0 0], name = "sblock", status = Moving}

zBlock = Block {pos = V2 0 0, poss = [V2 (-1) (-1), V2 0 (-1), V2 0 0, V2 1 0], name = "zblock", status = Moving}

lBlock = Block {pos = V2 0 0, poss = [V2 0 (-2), V2 0 (-1), V2 0 0, V2 1 0], name = "lblock", status = Moving}

jBlock = Block {pos = V2 0 0, poss = [V2 0 (-2), V2 0 (-1), V2 0 0, V2 (-1) 0], name = "jblock", status = Moving}

data Game = Game
  { cols :: Int,
    rows :: Int,
    block :: Block,
    wall :: [Brick],
    gameover :: Bool,
    counter :: Int,
    score :: Int,
    gen :: Random.StdGen
  }
  deriving (Show)

initialGame :: Game
initialGame =
  Game
    { cols = 16,
      rows = 20,
      block = lBlock,
      wall = [],
      gameover = False,
      counter = 0,
      score = 0,
      gen = undefined
    }

--
-- Move the block of a game; keep the current if the move is invalid; mark as Dropped as into the wall.
moveGame :: TetrisDirection -> Game -> Game
moveGame dir game = game {block = setDropped . keepFromWall . keepInBounds . moveBlock dir $ curBlock}
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
-- Drop the block of a game until it is Dropped in the wall.
freeFall :: Game -> Game
freeFall g = case status $ block g of
  Dropped -> g
  Moving -> freeFall $ moveGame TetrisDown g

--
-- add a block to the wall
buildWall :: Game -> Game
buildWall g = if status b == Dropped then g {wall = w ++ map toBrick (poss b)} else g
  where
    w = wall g
    b = block g
    toBrick p = Brick (p + pos b) (name b)

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
-- periodic action.
tickGame :: Game -> Game
tickGame g
  | isGameOver g = g {gameover = True}
  | status (block g) == Dropped = newBlock . collapseWall . buildWall $ g
  | otherwise = tick 1 . moveGame TetrisDown $ g

--
-- ticker.
tick :: Int -> Game -> Game
tick i g = g {counter = counter g + i}

--
-- Create a new random block until it is inbounds
newBlock :: Game -> Game
newBlock g =
  let blocks = [iBlock, oBlock, tBlock, sBlock, zBlock, lBlock, jBlock]
      (pos, gen') = Random.randomR (V2 0 0, V2 (cols g - 1) 0) (gen g)
      (idx, gen'') = Random.randomR (0, 6) gen'
      block = (blocks !! idx) {pos = pos}
   in if inBounds block 0 (cols g) then g {block = block, gen = gen''} else newBlock g {gen = gen''}

--
-- Check if the game is over.
isGameOver :: Game -> Bool
isGameOver _ = False

--
-- remove full rows from the wall, increase score.
collapseWall :: Game -> Game
collapseWall g =
  let (dels, aboves, belows) = analyseWall g
      rowCnt = length $ nubBy (\b1 b2 -> (^. _y) (brPos b1) == (^. _y) (brPos b2)) dels

      dropBrick br = br {brPos = brPos br + V2 0 rowCnt}
      aboves' = map dropBrick aboves
   in g {wall = belows ++ aboves', score = score g + 10 ^ rowCnt}

--
-- analyse the wall into full rows, rows below full rows, rows above fullrows.
analyseWall :: Game -> ([Brick], [Brick], [Brick])
analyseWall g =
  let sorted = sortOn (Data.Ord.Down . (^. _y) . brPos) $ wall g
      grouped = groupBy (\b1 b2 -> brPos b1 ^. _y == brPos b2 ^. _y) sorted
      fulls = concat . filter ((== cols g) . length) $ grouped
      belows = concat . takeWhile ((/= cols g) . length) $ grouped
      aboves = (wall g \\ fulls) \\ belows
   in (fulls, aboves, belows)

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
initGame :: IO Game
initGame = do
  g <- Random.newStdGen
  return $ newBlock initialGame {gen = g}
