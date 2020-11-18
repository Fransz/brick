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
import Data.Maybe as Maybe (isNothing)
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
    name :: String
  }
  deriving (Show)

data Brick = Brick
  { brPos :: Pos,
    brName :: String
  }
  deriving (Show)

instance Eq Brick where
  (==) b1 b2 = brPos b1 == brPos b2

iBlock = Block {pos = V2 0 0, poss = [V2 0 (-2), V2 0 (-1), V2 0 0, V2 0 1], name = "iblock"}

oBlock = Block {pos = V2 0 0, poss = [V2 0 (-1), V2 1 (-1), V2 0 0, V2 1 0], name = "oblock"}

tBlock = Block {pos = V2 0 0, poss = [V2 (-1) (-1), V2 0 (-1), V2 0 0, V2 1 (-1)], name = "tblock"}

sBlock = Block {pos = V2 0 0, poss = [V2 0 (-1), V2 1 (-1), V2 (-1) 0, V2 0 0], name = "sblock"}

zBlock = Block {pos = V2 0 0, poss = [V2 (-1) (-1), V2 0 (-1), V2 0 0, V2 1 0], name = "zblock"}

lBlock = Block {pos = V2 0 0, poss = [V2 0 (-2), V2 0 (-1), V2 0 0, V2 1 0], name = "lblock"}

jBlock = Block {pos = V2 0 0, poss = [V2 0 (-2), V2 0 (-1), V2 0 0, V2 (-1) 0], name = "jblock"}

data Game = Game
  { cols :: Int,
    rows :: Int,
    block :: Maybe Block,
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
      block = Nothing,
      wall = [],
      gameover = False,
      counter = 0,
      score = 0,
      gen = undefined
    }

--
-- Move the block of a game if there is one.
moveGame :: TetrisDirection -> Game -> Game
moveGame dir game = case block game of
  Nothing -> game
  Just block -> case moveBlock dir game block of
    Nothing -> game {block = Nothing, wall = addToWall block (wall game)}
    Just block' -> game {block = Just block'}

--
-- Move a block. Returning Nothing if the move was not posible
moveBlock :: TetrisDirection -> Game -> Block -> Maybe Block
moveBlock dir game block =
  let block' = case dir of
        TetrisLeft -> moveCenter block (V2 (-1) 0)
        TetrisRight -> moveCenter block (V2 1 0)
        TetrisDown -> moveCenter block (V2 0 1)
        TetrisUp -> rotate block
   in if inWall block' $ wall game ++ ground game
        then Nothing
        else if inBounds block' 0 (cols game) then Just block' else Just block

--
-- Move the center position of a block.
moveCenter :: Block -> V2 Int -> Block
moveCenter b d = b {pos = pos b + d}

--
-- rotate a block.
rotate :: Block -> Block
rotate b = b {poss = map perp $ poss b}

--
-- Drop the block of a game, if there is one, to the wall.
freeFall :: Game -> Game
freeFall game = case block game of
  Nothing -> game
  Just b ->
    let block' = fallWall b $ wall game ++ ground game
     in game {wall = addToWall block' $ wall game, block = Nothing}

--
-- drop a block to a wall. returning the block in its last position.
fallWall :: Block -> [Brick] -> Block
fallWall block wall =
  let block' = moveCenter block (V2 0 1)
   in if inWall block' wall then block else fallWall block' wall

--
-- add a block to a wall
addToWall :: Block -> [Brick] -> [Brick]
addToWall block wall = wall ++ map (\p -> Brick (p + pos block) (name block)) (poss block)

--
-- Calculate the ground of the game.
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
tickGame game
  | isGameOver game = game {gameover = True}
  | isNothing (block game) = newBlock . collapseWall $ game
  | otherwise = tick 1 . collapseWall . moveGame TetrisDown $ game

--
-- ticker.
tick :: Int -> Game -> Game
tick i g = g {counter = counter g + i}

--
-- Create a new random block.
newBlock :: Game -> Game
newBlock game =
  let (pos, g') = Random.randomR (V2 0 0, V2 (cols game - 1) 0) (gen game)
      (idx, g'') = Random.randomR (0, 6) g'
      blocks =
        [ iBlock {pos = pos},
          oBlock {pos = pos},
          tBlock {pos = pos},
          sBlock {pos = pos},
          zBlock {pos = pos},
          lBlock {pos = pos},
          jBlock {pos = pos}
        ]
   in game {block = Just (blocks !! idx), gen = g''}

--
-- Check if the game is over.
isGameOver :: Game -> Bool
isGameOver game = False

--
-- remove full rows from the wall, increase score.
collapseWall :: Game -> Game
collapseWall game =
  let (dels, aboves, belows) = analyseWall game
      rowCnt = length $ nubBy (\b1 b2 -> (^. _y) (brPos b1) == (^. _y) (brPos b2)) dels

      dropBrick br = br {brPos = brPos br + V2 0 rowCnt}
      aboves' = map dropBrick aboves
   in game {wall = belows ++ aboves', score = score game + 10 ^ rowCnt}

--
-- analyse the wall into full rows, rows below full rows, rows above fullrows.
analyseWall :: Game -> ([Brick], [Brick], [Brick])
analyseWall game =
  let sorted = sortOn (Data.Ord.Down . (^. _y) . brPos) $ wall game
      grouped = groupBy (\b1 b2 -> brPos b1 ^. _y == brPos b2 ^. _y) sorted
      fulls = concat . filter ((== cols game) . length) $ grouped
      belows = concat . takeWhile ((/= cols game) . length) $ grouped
      aboves = (wall game \\ fulls) \\ belows
   in (fulls, aboves, belows)

--
-- Map of all blocks, all positions with the blocks attr.
posNameMap :: Game -> Map.Map Pos String
posNameMap game = Map.fromList (maybe [] posNameTpl (block game) ++ posNameWall (wall game))

--
-- List off absolute positions of a block, in a tuple with the blocks attrName
posNameTpl :: Block -> [(Pos, String)]
posNameTpl b = map ((,name b) . (+ pos b)) (poss b)

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
